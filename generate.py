import urllib.request
import os
import sys
import datetime

# ranges are lists of range-objects, which must be sorted and must not overlap/neighbor each other if same type
#	=> use Ranges.fromRawList to sort and merge an arbitrary list of Range objects
# ranges map [first-last] to an integer [>= 0]
# invariant for ranges: (first >= 0) and (first <= last) and (last <= 2**32 - 1)

class Range:
	RangeFirst: int = 0
	RangeLast: int = 2**32 - 1

	def __init__(self, first: int, last: int, values: tuple[int]|int) -> None:
		if type(values) == int:
			values = (values,)
		if first < Range.RangeFirst or last > Range.RangeLast or first > last:
			raise RuntimeError('Malformed range encountered')
		if type(values) != tuple or len(values) == 0:
			raise RuntimeError('Malformed values encountered')
		self.first = first
		self.last = last
		self.values = values
	def __str__(self) -> str:
		return f'[{self.first:05x}-{self.last:05x}/{self.span()}] -> {self.values}'
	def __repr__(self) -> str:
		return self.__str__()
	def merge(self, other: 'Range') -> 'Range':
		if self.values != other.values:
			raise RuntimeError('Cannot merge ranges of different value')
		return Range(min(self.first, other.first), max(self.last, other.last), self.values)
	def split(self, size: int) -> tuple['Range', 'Range']:
		if self.first + size > self.last:
			raise RuntimeError(f'Can only split a range within itself [{self.span()}/{size}]')
		left = Range(self.first, self.first + size - 1, self.values)
		right = Range(self.first + size, self.last, self.values)
		return left, right
	def span(self) -> int:
		return (self.last - self.first + 1)
	def neighbors(self, other: 'Range') -> bool:
		return (self.last + 1 == other.first or self.first - 1 == other.last)
	def overlap(self, other: 'Range') -> bool:
		return (self.last >= other.first and self.first <= other.last)

class LookupType:
	def __init__(self) -> None:
		self._kind = ''
		self._typeName = ''
		self._values = []
		self._default = 0
	@staticmethod
	def boolType() -> 'LookupType':
		out = LookupType()
		out._kind = 'bool'
		out._typeName = 'bool'
		out._default = 0
		return out
	@staticmethod
	def intType(defValue: int, intType: str) -> 'LookupType':
		out = LookupType()
		out._kind = 'int'
		out._typeName = intType
		out._default = defValue
		return out
	@staticmethod
	def listType(defValue: int, intType: list[int]) -> 'LookupType':
		smallest, largest = min(intType), max(intType)

		# lookup the smallest suitable type to be used
		for i in [8, 16, 32]:
			if smallest >= 0 and largest <= 2**i - 1:
				return LookupType.intType(defValue, f'uint{i}_t')
			elif smallest >= -2**(i - 1) and largest <= 2**(i - 1) - 1:
				return LookupType.intType(defValue, f'int{i}_t')
		raise RuntimeError('No datatype suitable for buffer found')
	@staticmethod
	def enumType(name: str, defValue: str, values: list[str]) -> 'LookupType':
		out = LookupType()
		out._kind = 'enum'
		out._typeName = f'{name}'
		out._values = values
		out._default = values.index(defValue)
		if len(out._values) == 0:
			raise RuntimeError(f'Enum [{name}] must not be empty')
		return out

	def typeName(self, raw: bool = False) -> str:
		if raw or self._kind != 'enum':
			return self._typeName
		return f'gen::{self._typeName}'
	def defValue(self) -> int:
		if self._kind == 'bool':
			return self._default
		if self._kind == 'int':
			return self._default
		if self._kind == 'enum':
			return self._default
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def staticLookup(self, index: int) -> str:
		if self._kind == 'bool':
			return 'true' if index > 0 else 'false'
		if self._kind == 'int':
			return str(index)
		if self._kind == 'enum':
			return f'gen::{self._typeName}::{self._values[index]}'
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def dynLookup(self, varName: str, forceCast: bool) -> str:
		if self._kind == 'bool':
			return f'({varName} != 0)'
		if self._kind == 'int':
			return (f'{self._typeName}({varName})' if forceCast else f'{varName}')
		if self._kind == 'enum':
			return f'static_cast<gen::{self._typeName}>({varName})'
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def enumValues(self) -> list[str]:
		if self._kind == 'enum':
			return self._values
		raise RuntimeError(f'Function undefined for [{self._kind}]')
	def bufferType(self) -> str:
		if self._kind == 'bool':
			return 'uint8_t'
		if self._kind == 'int':
			return self._typeName
		if self._kind == 'enum':
			if len(self._values) > 2**16:
				return 'uint32_t'
			elif len(self._values) > 2**8:
				return 'uint16_t'
			return 'uint8_t'
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def bufferTypeBounds(self) -> tuple[int, int]:
		typeName = self.bufferType()
		for i in [8, 16, 32]:
			if typeName == f'int{i}_t':
				return [-2**(i - 1), 2**(i - 1) - 1]
			elif typeName == f'uint{i}_t':
				return [0, 2**i - 1]
		raise RuntimeError(f'Unsupported buffer-type [{typeName}] encountered')

class StrHelp:
	@staticmethod
	def value(val: int, valIsChar: bool) -> str:
		if not valIsChar or val >= 0xffff or (val >= 0xd800 and val <= 0xdfff):
			return f'0x{val:05x}'
		if val >= 0x80:
			return f'U\'\\u{val:04x}\''
		return f'U{repr(chr(val))}'
	@staticmethod
	def indent(string: str, level: int = 1, startsAsNewLine: bool = True) -> str:
		out = ('\t' * level) if startsAsNewLine else ''

		# construct the indented string (only indent if the line is non-empty)
		for i in range(len(string)):
			out += string[i]
			if string[i] == '\n' and i + 1 < len(string) and string[i + 1] != '\n':
				out += '\t' * level
		return out
	@staticmethod
	def multiLines(string: str, excludeTrailing: bool = True) -> bool:
		if excludeTrailing:
			return (string[:-1].find('\n') >= 0)
		return (string.find('\n') >= 0)

class Ranges:
	@staticmethod
	def _appOrMerge(out: list[Range], other: Range) -> None:
		if len(out) > 0 and (out[-1].overlap(other) or (out[-1].neighbors(other) and out[-1].values == other.values)):
			out[-1] = out[-1].merge(other)
		else:
			out.append(other)
	@staticmethod
	def _setIteration(a: list[Range], b: list[Range], binaryOp: bool, fn) -> list[Range]:
		if binaryOp and (any(_a.values != (1,) for _a in a) or any(_b.values != (1,) for _b in b)):
			raise RuntimeError('Set operation is only defined for binary ranges')
		out: list[Range] = []

		# iterate over the two ranges and pass them to the callback
		aOff, bOff, aNext, bNext, nextProcessed = 0, 0, None, None, Range.RangeFirst
		while True:
			# check if the next-values need to be updated
			if aNext is None and aOff < len(a):
				aNext, aOff = a[aOff], aOff + 1
			if bNext is None and bOff < len(b):
				bNext, bOff = b[bOff], bOff + 1

			# check if the end has been reached
			if aNext is None and bNext is None:
				break
			aFirst, aLast = (Range.RangeLast, Range.RangeLast) if aNext is None else (aNext.first, aNext.last)
			bFirst, bLast = (Range.RangeLast, Range.RangeLast) if bNext is None else (bNext.first, bNext.last)
			
			# find the starting value to be used
			first = max(nextProcessed, min(aFirst, bFirst))

			# find the ending value to be used
			last = min(aLast, bLast)
			if first < aFirst and last >= aFirst and aNext is not None:
				last = aFirst - 1
			if first < bFirst and last >= bFirst and bNext is not None:
				last = bFirst - 1
			
			# invoke the callback and add the next range to the output
			val = fn(aNext.values if aFirst <= first else None, bNext.values if bFirst <= first else None)
			if val is not None:
				Ranges._appOrMerge(out, Range(first, last, val))
			nextProcessed = last + 1

			# check which of the two range's has been fully consumed
			if aLast == last:
				aNext = None
			if bLast == last:
				bNext = None
		return out
	@staticmethod
	def _anyUnionOperationFavFirst(a: tuple[int]|None, b: tuple[int]|None) -> tuple[int]|None:
		if a is None:
			return b
		return a
	@staticmethod
	def _unionOperation(a: tuple[int]|None, b: tuple[int]|None) -> tuple[int]|None:
		if a is None:
			return b
		if b is None:
			return a
		
		# perform merge to trigger an exception on invalid merges
		return Range(0, 0, a).merge(Range(0, 0, b)).values
	@staticmethod
	def _intersectOperation(a: tuple[int]|None, b: tuple[int]|None) -> tuple[int]|None:
		if a is None or b is None:
			return None
		return (1,)
	@staticmethod
	def _differenceOperation(a: tuple[int]|None, b: tuple[int]|None) -> tuple[int]|None:
		if b is None:
			return a
		return None

	@staticmethod
	def fromRawList(ranges: list[Range]) -> list[Range]:
		# sort the ranges
		ranges = sorted(ranges, key=lambda r : r.first)

		# merge any neighboring/overlapping ranges of the same type
		out: list[Range] = []
		for r in ranges:
			Ranges._appOrMerge(out, r)
		return out
	@staticmethod
	def wellFormed(ranges: list[Range]) -> None:
		for i in range(len(ranges)):
			if i > 0 and ranges[i - 1].first > ranges[i].first:
				raise RuntimeError('Order of ranges violation encountered')
			if i > 0 and ranges[i - 1].last + 1 > ranges[i].first:
				raise RuntimeError('Overlapping ranges encountered')
	@staticmethod
	def split(ranges: list[Range], startOfOther: int) -> tuple[list[Range], list[Range]]:
		left: list[Range] = []
		right: list[Range] = []

		# copy the ranges to the left until the cut-off point has been reached
		for i in range(len(ranges)):
			if ranges[i].first >= startOfOther:
				right.append(ranges[i])
			elif ranges[i].last < startOfOther:
				left.append(ranges[i])
			else:
				l, r = ranges[i].split(startOfOther - ranges[i].first)
				left.append(l)
				right.append(r)
		return (left, right)
	@staticmethod
	def fill(ranges: list[Range], fillValue: tuple[int]|int, leftEdge: int, rightEdge: int) -> list[Range]:
		if type(fillValue) == int:
			fillValue = (fillValue,)
		out: list[Range] = []
		if len(ranges) == 0:
			return [Range(leftEdge, rightEdge, fillValue)]

		# check if the lower edge is invalid
		if leftEdge > ranges[0].first:
			leftEdge = ranges[0].first

		# copy and patch all intermediate holes
		for range in ranges:
			if leftEdge != range.first:
				out.append(Range(leftEdge, range.first - 1, fillValue))
			out.append(range)
			leftEdge = out[-1].last + 1

		# check if the upper edge needs to be flooded
		if rightEdge > ranges[-1].last:
			out.append(Range(ranges[-1].last + 1, rightEdge, fillValue))
		return out
	@staticmethod
	def limitSize(ranges: list[Range], maxSize: int) -> list[Range]:
		# copy the ranges over and check if they need to be broken apart
		out: list[Range] = []
		for i in range(len(ranges)):
			out.append(ranges[i])
			while out[-1].span() > maxSize:
				out[-1], r = out[-1].split(maxSize)
				out.append(r)
		return out
	@staticmethod
	def filter(ranges: list[Range], drop: tuple[int]|int) -> list[Range]:
		if type(drop) == int:
			drop = (drop,)
		out: list[Range] = []
		for i in range(len(ranges)):
			if ranges[i].values != drop:
				out.append(ranges[i])
		return out
	@staticmethod
	def cluster(ranges: list[Range], cluster: Range) -> tuple[list[Range], list[Range]]:
		if cluster.first >= len(ranges) or cluster.last >= len(ranges):
			raise RuntimeError('Invalid cluster specified')
		out = ranges[:cluster.first] + [Range(ranges[cluster.first].first, ranges[cluster.last].last, cluster.values)] + ranges[cluster.last + 1:]
		return out, ranges[cluster.first:cluster.last + 1]

	@staticmethod
	def merge(a: list[Range], b: list[Range], inFavorOfA: bool) -> list[Range]:
		if inFavorOfA:
			return Ranges._setIteration(a, b, False, Ranges._anyUnionOperationFavFirst)
		return Ranges._setIteration(b, a, False, Ranges._anyUnionOperationFavFirst)
	@staticmethod
	def union(a: list[Range], b: list[Range]) -> list[Range]:
		return Ranges._setIteration(a, b, False, Ranges._unionOperation)
	@staticmethod
	def intersect(a: list[Range], b: list[Range]) -> list[Range]:
		return Ranges._setIteration(a, b, True, Ranges._intersectOperation)
	@staticmethod
	def difference(a: list[Range], b: list[Range]) -> list[Range]:
		return Ranges._setIteration(a, b, True, Ranges._differenceOperation)
	@staticmethod
	def complement(a: list[Range]) -> list[Range]:
		out: list[Range] = []
		lastEnd = Range.RangeFirst - 1
		
		# invert the range including between 0 and the range
		for i in range(len(a)):
			if a[i].values != (1,):
				raise RuntimeError('Complement operation only defined for binary ranges')
			if a[i].first > lastEnd + 1:
				out.append(Range(lastEnd + 1, a[i].first - 1, 1))
			lastEnd = a[i].last

		# add the final inversion to the 2**32
		if lastEnd + 1 < Range.RangeLast:
			out.append(Range(lastEnd + 1, Range.RangeLast, 1))
		return out
	@staticmethod
	def translate(a: list[Range], assignValue) -> list[Range]:
		out: list[Range] = []
		for r in a:
			for i in range(r.first, r.last + 1):
				Ranges._appOrMerge(out, Range(i, i, assignValue(i, r.values)))
		return out

class GeneratedFile:
	def __init__(self, path: str, addBinSearch: bool) -> None:
		self._path = path
		self._file = None
		self._addBinSearch = addBinSearch
		self._hadFirstBlock = False
		self._atStartOfLine = False
		self._indented = False
	def __enter__(self) -> 'GeneratedFile':
		self._file = open(self._path, mode='w', encoding='ascii')
		self._atStartOfLine = True

		# write the file header
		self.writeln('#pragma once')
		self.writeln('')
		self.writeln('#include <cinttypes>')
		self.writeln('#include <utility>')
		self.writeln('')
		self._writeComment('This is an automatically generated file and should not be modified.\n'
				  + 'All data are based on the lastest information provided by the unicode character database.\n'
				  + 'Source: https://www.unicode.org/Public/UCD/latest\n'
				  + f'Generated: {datetime.datetime.today().strftime('%Y-%m-%d %H:%M')}', False)
		self.writeln('namespace cp::detail::gen {')
		self._indented = True

		# check if the binary search template should be added
		if not self._addBinSearch:
			return self
		self._hadFirstBlock = True
		self.writeln('template <class Type, size_t N>')
		self.writeln('inline constexpr size_t BinarySearch(char32_t cp, const Type (&data)[N]) {')
		self.writeln('\tsize_t left = 0, right = N - 1;')
		self.writeln('\twhile (left < right) {')
		self.writeln('\t\tsize_t center = (left + right + 1) / 2;')
		self.writeln('\t\tif (cp < data[center])')
		self.writeln('\t\t\tright = center - 1;')
		self.writeln('\t\telse')
		self.writeln('\t\t\tleft = center;')
		self.writeln('\t}')
		self.writeln('\treturn left;')
		self.writeln('}')
		return self
	def __exit__(self, *args) -> None:
		if self._file is not None:
			self._file.write('}\n')
			self._file.close()
		self._file = None
		return False
	def _writeComment(self, msg: str, blockHeader: bool) -> None:
		if blockHeader:
			msg = msg.replace('\n', '\n\t*\t')
			self._file.write(f'\t/* {msg} */\n')
		else:
			msg = msg.replace('\n', '\n*\t')
			self._file.write(f'/*\n*\t{msg}\n*/\n')
	def beginBlock(self, msg: str) -> None:
		# ensure an indentation of two newlines to the last block
		if not self._atStartOfLine:
			self._file.write('\n')
		if self._hadFirstBlock:
			self._file.write('\n\n')
		self._hadFirstBlock = True
		self._writeComment(msg, True)
	def write(self, msg: str) -> None:
		if len(msg) == 0:
			return
		
		# check if the last text ended in a new-line and add the indentation (only if the next line is not empty)
		if self._atStartOfLine and msg[0] != '\n' and self._indented:
			self._file.write('\t')

		# construct the indented string
		if self._indented:
			msg = StrHelp.indent(msg, 1, False)
		
		# write the indented string out and check if it ended in a newline
		self._file.write(msg)
		self._atStartOfLine = (msg[-1] == '\n')
	def writeln(self, msg: str) -> None:
		self.write(f'{msg}\n')
	def binSearch(self, data: str) -> str:
		if not self._addBinSearch:
			raise RuntimeError('BinarySearch has not been created')
		return f'gen::BinarySearch(cp, {data})'

class CodeGen:
	# final stored size will be smaller by one as sizes of 0 cannot exist
	MaxBinarySearchSize = 0x10000

	ThresholdDensity = 1.0 / 2.0
	ThresholdClusterSize = 8
	MaxRangesUntilDedicatedUpper = 48
	MaxRangesUntilDedicatedAscii = 8
	NestedBinSearch = False
	ListConsideredSparse = 0.5

	def __init__(self, file: GeneratedFile, blockName: str, desc: str) -> None:
		self._file = file
		self._file.beginBlock(desc)
		self._blockName = blockName
		self._varIndex = 0
		self._bufIndex = 0
		self._inBinSearch = 0

	def _buffer(self, hint: str, data: list[int], tp: LookupType|None) -> str:
		# check if the type should be determined to fit the data
		if tp is None:
			tp = LookupType.listType(0, data)

		# otherwise check if the type is suited for the data
		else:
			lowerBound, upperBound = tp.bufferTypeBounds()
			if any(d < lowerBound or d > upperBound for d in data):
				raise RuntimeError(f'Type [{tp.typeName()}] is not suited for data')

		# construct the buffer-name
		name = f'{self._blockName}Buf{self._bufIndex}{hint}'
		self._bufIndex += 1

		# check if the values should be written as hex (if more than a quarter of the values are larger than 256)
		printAsHex = (sum(1 for d in data if d >= 256) * 4 > len(data))

		# slightly align the count to get a closer resembles of rectangles
		valsPerLine = 24 if printAsHex else 32
		estimatedLines = (len(data) + valsPerLine - 1) // valsPerLine
		valsPerLine = (len(data) + estimatedLines - 1) // estimatedLines
	
		# write the header and the data out
		self._file.write(f'static constexpr {tp.bufferType()} {name}[{len(data)}] = {{\n\t')
		for i in range(len(data)):
			if i > 0:
				self._file.write(f',{"\n\t" if (i % valsPerLine) == 0 else ""}')
			self._file.write(f' {data[i]:#06x}' if printAsHex else f'{data[i]:3}')

		# close the buffer-string and return the access name
		self._file.writeln('\n};')
		return f'gen::{name}'
	def _varName(self, hint: str) -> str:
		out = f'var{self._varIndex}{hint}'
		self._varIndex += 1
		return out
	def _condition(self, ranges: list[Range], checkLower: bool, checkUpper: bool, varName: str) -> str:
		out = '('

		# iterate over the ranges to be tested and add them to the condition (check greater/less equal before equality, to ensure open sides are handled properly)
		for i in range(len(ranges)):
			if i > 0:
				out += ' || '
				
			first, last = f'{ranges[i].first:#06x}', f'{ranges[i].last:#06x}'
			if not checkLower and i == 0:
				out += f'{varName} <= {last}'
			elif not checkUpper and i + 1 == len(ranges):
				out += f'{varName} >= {first}'
			elif ranges[i].span() == 1:
				out += f'{varName} == {first}'
			elif len(ranges) > 1:
				out += f'({varName} >= {first} && {varName} <= {last})'
			else:
				out += f'{varName} >= {first} && {varName} <= {last}'
		return out + ')'
	def _density(self, ranges: list[Range]) -> list[tuple[int, int]]:
		# initialize the cluster-map [(first, last, ranges, chars)]
		clusters = []
		for i in range(len(ranges)):
			clusters.append((i, i, 1, ranges[i].span()))

		# iteratively merge clusters, if their combined density lies beneath the threshold (density = ranges / chars, higher is better)
		while len(clusters) > 1:
			# look for the two neighboring clusters, which result in the highest density
			best, bestDensity = None, 0.0
			for i in range(1, len(clusters)):
				density = (clusters[i - 1][2] + clusters[i][2]) / (clusters[i - 1][3] + clusters[i][3])
				if best is None or density > bestDensity:
					best = i - 1
					bestDensity = density

			# check if the pair can be merged or if the threshold has not been reached
			if bestDensity < CodeGen.ThresholdDensity:
				break
			l, r = clusters[best], clusters[best + 1]
			clusters[best] = (l[0], r[1], l[2] + r[2], l[3] + r[3])
			clusters = clusters[:best + 1] + clusters[best + 2:]

		# remove all clusters, which either did not reach the density or chars threshold
		index = 0
		while index < len(clusters):
			if clusters[index][3] >= CodeGen.ThresholdClusterSize and (clusters[index][2] / clusters[index][3]) >= CodeGen.ThresholdDensity:
				index += 1
			else:
				clusters = clusters[:index] + clusters[index + 1:]

		# transform the clusters to the required output format (will be sorted as the algorithm does not reorder them)
		return [(clusters[i][0], clusters[i][1]) for i in range(len(clusters))]
	def _analyze(self, ranges: list[Range]) -> map:
		out = {}
		for i in range(len(ranges)):
			if ranges[i].values not in out:
				out[ranges[i].values] = []
			out[ranges[i].values].append(i)
		return out
	def _rangeFromList(self, data: list) -> list[Range]:
		return Ranges.fromRawList([Range(i, i, data[i]) for i in range(len(data))])

	def _singleRangeBinarySearch(self, ranges: list[Range], hint: str, tp: LookupType, inVarName: str, outVarName: str|None) -> str:
		defValue = None
		
		# check if the last value will be the default value (due to the size-constraints, any too large last-values can produce exeedingly many values)
		if (ranges[-1].span() // CodeGen.MaxBinarySearchSize) >= len(ranges):
			defValue = ranges[-1].values
			ranges = ranges[:-1] + [Range(ranges[-1].first, ranges[-1].first + CodeGen.MaxBinarySearchSize - 1, defValue)]

		# update the ranges to adhere to the size-constraints
		ranges: list[Range] = Ranges.limitSize(ranges, CodeGen.MaxBinarySearchSize)

		# lookup the base value to be used as default and filter it from the ranges
		counts = self._analyze(ranges)
		if defValue is None:
			for value in counts:
				if defValue is None or len(counts[defValue]) < len(counts[value]):
					defValue = value
		ranges = Ranges.filter(ranges, defValue)
	
		# check if there are more than two values, in which case a lookup for the values is required
		indexVar, lookupValueCode = self._varName(f'{hint}Index'), None
		if len(counts) > 2:
			tempRanges = self._rangeFromList([r.values for r in ranges])
			lookupValueCode = self._singleRangeLookup(tempRanges, f'{hint}Value', tp, indexVar, outVarName)
		elif outVarName is None:
			lookupValueCode = f'return {tp.staticLookup(ranges[0].values[0])};\n'
		else:
			lookupValueCode = f'{outVarName} = {tp.staticLookup(ranges[0].values[0])};\n'

		# create the lookup code for the ranges sizes (store the size smaller by 1 to allow 65536 to be written)
		lookupSizeOutVar = self._varName(f'{hint}SizeResult')
		sizeBufferList = [r.span() - 1 for r in ranges]
		sizeBufferType = LookupType.listType(0, sizeBufferList)
		lookupSizeCode = self._singleRangeLookup(self._rangeFromList(sizeBufferList), f'{hint}Size', sizeBufferType, indexVar, lookupSizeOutVar)

		# write the buffer for the first-values out (must be a buffer, as it will be passed
		# to the binary-search and will hopefully be complex enough to require a binary-search)
		firstBufferName = self._buffer(f'{hint}Start', [r.first for r in ranges], None)

		# setup the code to perform the binary-search (check input-var greater than size, as size is reduced by one)
		findValue = f'{sizeBufferType.typeName()} { lookupSizeOutVar} = 0;\n'
		findValue += f'size_t {indexVar} = {self._file.binSearch(firstBufferName)};\n'
		
		# add the code to fetch the size
		findValue += lookupSizeCode

		# add the negative size condition
		findValue += f'if ({inVarName} - {firstBufferName}[{indexVar}] > {lookupSizeOutVar})\n'
		if outVarName is None:
			findValue += f'\treturn {tp.staticLookup(defValue[0])};\n'
		else:
			findValue += f'\t{outVarName} = {tp.staticLookup(defValue[0])};\n'

		# add the code to perform the value-lookup
		if outVarName is None:
			findValue += lookupValueCode
		elif StrHelp.multiLines(lookupValueCode):
			findValue += 'else {\n' + StrHelp.indent(lookupValueCode) + '}\n'
		else:
			findValue += f'else\n\t{lookupValueCode}'
		return findValue
	def _singleRangeRawBuffer(self, ranges: list[Range], hint: str, tp: LookupType, inVarName: str, outVarName: str|None) -> str:
		# check if the buffer is a one-to-one mapping and can just be ignored
		if all(ranges[i].span() == 1 and ranges[i].values[0] == i for i in range(len(ranges))):
			accessString = tp.dynLookup(inVarName, True)

		# create the buffer to be written out and setup the access string
		else:
			bufName = self._buffer(hint, [r.values[0] for r in ranges for _ in range(r.first, r.last + 1)], tp)
			accessString = tp.dynLookup(f'{bufName}[{inVarName}]', False)

		# return the final assignment/return string
		if outVarName is None:
			return f'return {accessString};\n'
		return f'{outVarName} = {accessString};\n'
	def _singleRangeCode(self, ranges: list[Range], hint: str, tp: LookupType, inVarName: str, outVarName: str|None) -> str:
		# perform density-clustering to detect what areas might benefit from buffers
		clusters, clusterAccess = self._density(ranges), []

		# construct the cluster-buffer
		if len(clusters) > 0:
			clusterShift, clusterData = 0, []

			# iterate over the clusters and remove them from the range and write the data to the buffer
			for cluster in clusters:
				ranges, dropped = Ranges.cluster(ranges, Range(cluster[0] - clusterShift, cluster[1] - clusterShift, (0, len(clusterAccess))))
				clusterShift += len(dropped) - 1
				offset = len(clusterData) - dropped[0].first
				clusterData += [r.values[0] for r in dropped for _ in range(r.first, r.last + 1)]

				# setup the index into the buffer
				clusterAccess.append(inVarName)
				if offset != 0:
					clusterAccess[-1] += (' + ' if offset > 0 else ' - ') + f'{abs(offset)}'

			# write the buffer out and patch the access-strings
			bufferName = self._buffer(f'{hint}Cluster', clusterData, tp)
			for i in range(len(clusterAccess)):
				clusterAccess[i] = tp.dynLookup(f'{bufferName}[{clusterAccess[i]}]', False)
				if outVarName is None:
					clusterAccess[i] = f'return {clusterAccess[i]};\n'
				else:
					clusterAccess[i] = f'{outVarName} = {clusterAccess[i]};\n'

		# analyze the remaining ranges
		state, active = self._analyze(ranges), { i for i in range(len(ranges)) }

		# iterate until all ranges have been processed/inserted
		lookupCode, statementCount = '', -1
		while True:
			if outVarName is not None:
				statementCount += 1

			# look for the ranges with the largest number of characters, which consists of the fewest number of ranges
			bestValues, bestRanges, bestChars = None, 0, 0
			for k, ra in state.items():
				if bestValues is not None and bestRanges < len(ra):
					continue
				chars = sum(ranges[r].span() for r in ra)
				if bestValues is None or bestRanges > len(ra) or chars > bestChars:
					bestValues, bestRanges, bestChars = k, len(ra), chars

			# remove the selected group from the state and remove the corresponding ranges from the active set
			selected: list[int] = state.pop(bestValues)
			for i in selected:
				active.remove(i)

			# setup the code-line for the final value
			if len(bestValues) > 1:
				selectedValue = clusterAccess[bestValues[1]]
			else:
				selectedValue = ('return' if outVarName is None else f'{outVarName} =') + f' {tp.staticLookup(bestValues[0])};\n'
			
			# check if these were the last ranges
			if len(active) == 0:
				if statementCount > 0:
					lookupCode += 'else\n\t'
				lookupCode += selectedValue
				return lookupCode
			
			# iterate over the selected ranges and construct the ranges to be checked (only the actually relevant ranges; abuse values as char-count)
			actual: list[Range] = [Range(ranges[selected[0]].first, ranges[selected[0]].last, ranges[selected[0]].span())]
			for i in range(1, len(selected)):
				found = False
				for j in range(selected[i - 1] + 1, selected[i]):
					if j in active:
						found = True
						break
				if found:
					actual.append(Range(ranges[selected[i]].first, ranges[selected[i]].last, ranges[selected[i]].span()))
				else:
					actual[-1] = Range(actual[-1].first, ranges[selected[i]].last, actual[-1].values[0] + ranges[selected[i]].span())

			# check if the upper or lower bounds need to be checked
			checkLower, checkUpper = any(i in active for i in range(selected[0])), any(i in active for i in range(selected[-1] + 1, len(ranges)))

			# reorganize the conditions to have all larger spans at the start
			sortedActual: list[Range] = []
			lastSorted = None
			if not checkLower:
				sortedActual.append(actual[0])
				actual = actual[1:]
			if not checkUpper:
				lastSorted = actual[-1]
				actual = actual[:-1]

			# extract all ranges which contain more than two chars
			sortedActual += sorted([a for a in actual if a.values[0] > 2], key=lambda x: -x.values[0])

			# add all remaining single checks, but sorted in ascending order
			actual = [a for a in actual if a.values[0] == 1] + [Range(a.first, a.first, 1) for a in actual if a.values[0] == 2] + [Range(a.last, a.last, 1) for a in actual if a.values[0] == 2]
			sortedActual += sorted(actual, key=lambda x: x.first)

			# add the last element back
			if lastSorted is not None:
				sortedActual.append(lastSorted)

			# create the conditional statement (limit its size to prevent overlong lines)
			perceivedSizeRemaining, index = sum((2 if a.values[0] > 1 else 1) for a in sortedActual), 0
			while index < len(sortedActual):
				# compute the number of entries to add to the statement to ensure visual consistency with the if-statements
				if perceivedSizeRemaining > 6:
					count, perceivedGoal, perceived = 0, min(perceivedSizeRemaining // 2, 6), 0
					while perceived < perceivedGoal:
						perceived += (2 if sortedActual[index + count].values[0] > 1 else 1)
						count += 1
					perceivedSizeRemaining -= perceived
				else:
					perceivedSizeRemaining, count = 0, len(sortedActual) - index

				# check if the bounds of this statement need to be checked and construct the code for it
				thisLower, thisUpper = (checkLower or index > 0), (checkUpper or index + count < len(sortedActual))
				lookupCode += ("else if " if statementCount > 0 else "if ") + self._condition(sortedActual[index:index + count], thisLower, thisUpper, inVarName)
				if StrHelp.multiLines(selectedValue):
					lookupCode += ' {\n' + StrHelp.indent(selectedValue) + '}\n'
				else:
					lookupCode += '\n' + StrHelp.indent(selectedValue)
				index += count
	def _singleRangeLookup(self, ranges: list[Range], hint: str, tp: LookupType, inVarName: str, outVarName: str|None) -> str:
		if any(len(r.values) != 1 for r in ranges):
			raise RuntimeError('Integer ranges must map exactly to a single value')
		
		# check if this is a simple lookup, as code lookup otherwise does not make sense (simple: low spread/few ranges)
		rangeCoveredChars, actualRangeChars = (ranges[-1].last - ranges[0].first + 1), sum(r.span() for r in ranges)
		if len(ranges) < 40 or (rangeCoveredChars <= 1.75 * actualRangeChars and rangeCoveredChars <= 3.0 * len(ranges)):
			return self._singleRangeCode(ranges, hint, tp, inVarName, outVarName)
		
		# check if a binary search can be performed or if a buffer-lookup should be done
		if self._inBinSearch == 0 or CodeGen.NestedBinSearch:
			self._inBinSearch += 1
			out = self._singleRangeBinarySearch(ranges, hint, tp, inVarName, outVarName)
			self._inBinSearch -= 1
			return out
		return self._singleRangeRawBuffer(ranges, hint, tp, inVarName, outVarName)

	def addEnum(self, enum: LookupType) -> None:
		# write the enum out
		self._file.write(f'enum class {enum.typeName(True)} : {enum.bufferType()} {{\n\t')
		self._file.writeln(",\n\t".join(enum.enumValues()))
		self._file.writeln('};')
	def intFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range]) -> None:
		print(f'Creating integer-lookup {fnName}...')

		# validate the ranges and fill it
		Ranges.wellFormed(ranges)
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# break the ranges into three parts [ascii][0x80 - 0xffff][0x10000 - ...] (ascii for fast common performance,
		# [0x80 - 0xffff] to allow a binary search to potentially fit all start indices into 16-bit, and the rest)
		asciiRanges, ranges = Ranges.split(ranges, 0x80)
		lowerRanges, upperRanges = Ranges.split(ranges, 0x10000)

		# check if the upper ranges are trivial, in which case they can be merged (only merge with ascii-range,
		# if only a single range exists, as it might otherwise still benefit from separate handling)
		if len(lowerRanges) + len(upperRanges) < CodeGen.MaxRangesUntilDedicatedUpper or len(lowerRanges) == 1 or len(upperRanges) == 1:
			lowerRanges, upperRanges = ranges, []
		if len(asciiRanges) + len(lowerRanges) < CodeGen.MaxRangesUntilDedicatedAscii:
			asciiRanges, lowerRanges = [], Ranges.union(asciiRanges, ranges)

		# create the lookup code for the three ranges
		asciiCode = ('' if len(asciiRanges) == 0 else self._singleRangeLookup(asciiRanges, 'Ascii', lookupType, 'cp', None))
		lowerCode = ('' if len(lowerRanges) == 0 else self._singleRangeLookup(lowerRanges, 'Low', lookupType, 'cp', None))
		higherCode = ('' if len(upperRanges) == 0 else self._singleRangeLookup(upperRanges, 'High', lookupType, 'cp', None))

		# patch the lookup code with the corresponding conditions
		if asciiCode != '' and (lowerCode != '' or higherCode != ''):
			if StrHelp.multiLines(asciiCode):
				asciiCode = f'if (cp < 0x80) {{\n{StrHelp.indent(asciiCode)}}}\n'
			else:
				asciiCode = f'if (cp < 0x80)\n{StrHelp.indent(asciiCode)}'
		if lowerCode != '' and higherCode != '':
			if StrHelp.multiLines(lowerCode):
				lowerCode = f'if (cp < 0x10000) {{\n{StrHelp.indent(lowerCode)}}}\n'
			else:
				lowerCode = f'if (cp < 0x10000)\n{StrHelp.indent(lowerCode)}'

		# generate the actual function and insert the final code
		self._file.writeln(f'inline constexpr {lookupType.typeName()} {fnName}(char32_t cp) {{')
		if asciiCode != '':
			self._file.write(StrHelp.indent(asciiCode))
		if lowerCode != '':
			self._file.write(StrHelp.indent(lowerCode))
		if higherCode != '':
			self._file.write(StrHelp.indent(higherCode))
		self._file.writeln('}')
	def listFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range]) -> None:
		print(f'Creating list-lookup {fnName}...')

		# validate the ranges and fill it
		Ranges.wellFormed(ranges)
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# check if a sparse buffer should be created (data-buffer contains either single value, or null, followed by
		#	size, followed by values; null-value also needs special encoding) or simply write size, and data out
		sparseBuffer = (sum(1 for r in ranges if len(r.values) > 1) <= len(ranges) * CodeGen.ListConsideredSparse)

		# create the data buffer for the ranges and remap the ranges to indices into the data-buffer
		dataBuffer, dataIndex = [], {}
		for i in range(len(ranges)):
			values = ranges[i].values

			# check if the value needs to be added to the buffer
			if values not in dataIndex:
				dataIndex[values] = len(dataBuffer)
				if not sparseBuffer:
					dataBuffer += [len(values)] + [v for v in values]
				elif len(values) == 1 and values[0] != 0:
					dataBuffer.append(values[0])
				else:
					dataBuffer += [0, len(values)] + [v for v in values]

			# map the range to the index into the value buffer
			ranges[i] = Range(ranges[i].first, ranges[i].last, dataIndex[values])

		# write the data buffer out
		dataBufferName = self._buffer('Data', dataBuffer, lookupType)

		# break the ranges into three parts [ascii][0x80 - 0xffff][0x10000 - ...] (ascii for fast common performance,
		# [0x80 - 0xffff] to allow a binary search to potentially fit all start indices into 16-bit, and the rest)
		asciiRanges, ranges = Ranges.split(ranges, 0x80)
		lowerRanges, upperRanges = Ranges.split(ranges, 0x10000)

		# check if the upper ranges are trivial, in which case they can be merged (only merge with ascii-range,
		# if only a single range exists, as it might otherwise still benefit from separate handling)
		if len(lowerRanges) + len(upperRanges) < CodeGen.MaxRangesUntilDedicatedUpper or len(lowerRanges) == 1 or len(upperRanges) == 1:
			lowerRanges, upperRanges = ranges, []
		if len(asciiRanges) + len(lowerRanges) < CodeGen.MaxRangesUntilDedicatedAscii:
			asciiRanges, lowerRanges = [], Ranges.union(asciiRanges, ranges)

		# create the lookup code for the three ranges
		indexVarName = self._varName('DataIndex')
		asciiCode = ('' if len(asciiRanges) == 0 else self._singleRangeLookup(asciiRanges, 'Ascii', lookupType, 'cp', indexVarName))
		lowerCode = ('' if len(lowerRanges) == 0 else self._singleRangeLookup(lowerRanges, 'Low', lookupType, 'cp', indexVarName))
		upperCode = ('' if len(upperRanges) == 0 else self._singleRangeLookup(upperRanges, 'High', lookupType, 'cp', indexVarName))

		# patch the lookup code with the corresponding conditions
		if asciiCode != '' and (lowerCode != '' or upperCode != ''):
			if StrHelp.multiLines(asciiCode):
				asciiCode = f'if (cp < 0x80) {{\n{StrHelp.indent(asciiCode)}}}\n'
			else:
				asciiCode = f'if (cp < 0x80)\n{StrHelp.indent(asciiCode)}'
		if lowerCode != '' and upperCode != '':
			if StrHelp.multiLines(lowerCode):
				lowerCode = ('else ' if asciiCode != '' else '') + f'if (cp < 0x10000) {{\n{StrHelp.indent(lowerCode)}}}\n'
			else:
				lowerCode = ('else ' if asciiCode != '' else '') + f'if (cp < 0x10000)\n{StrHelp.indent(lowerCode)}'
		if upperCode != '':
			if StrHelp.multiLines(upperCode):
				upperCode = f'else {{\n{StrHelp.indent(upperCode)}}}\n'
			else:
				upperCode = f'else\n{StrHelp.indent(upperCode)}'
		elif lowerCode != '':
			if StrHelp.multiLines(lowerCode):
				lowerCode = f'else {{\n{StrHelp.indent(lowerCode)}}}\n'
			else:
				lowerCode = f'else\n{StrHelp.indent(lowerCode)}'

		# generate the actual function and index evaluation
		self._file.writeln(f'inline constexpr std::pair<size_t, const {lookupType.typeName()}*> {fnName}(char32_t cp) {{')
		self._file.writeln(f'\tsize_t {indexVarName} = 0;')
		if asciiCode != '':
			self._file.write(StrHelp.indent(asciiCode))
		if lowerCode != '':
			self._file.write(StrHelp.indent(lowerCode))
		if upperCode != '':
			self._file.write(StrHelp.indent(upperCode))

		# add the logic to extract the size and data
		if sparseBuffer:
			self._file.writeln(f'\tif ({dataBufferName}[{indexVarName}] != 0)')
			self._file.writeln(f'\t\treturn {{ 1, {dataBufferName} + {indexVarName} }};')
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName} + 1]), {dataBufferName} + {indexVarName} + 2 }};')
		else:
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName}]), {dataBufferName} + {indexVarName} + 1 }};')
		self._file.writeln('}')

class ParsedFile:
	def _parseLine(self, line: str) -> None:
		missing = ('@missing:' in line)

		# check if this is a missing line
		if missing:
			_, line = line.split('@missing:')

		# remove any comments and split the line and strip all entries
		fields = [s.strip() for s in line.split('#')[0].split(';')]

		# validate the field count
		if fields == ['']:
			return None
		if len(fields) < 2:
			raise RuntimeError(f'Line with an invalid field count encountered [{fields[0]}]')
		cp, fields = fields[0], fields[1:]

		# expand the unicode range
		if '..' not in cp:
			return [missing, int(cp, 16), int(cp, 16), fields]
		begin, last = cp.split('..')
		return [missing, int(begin, 16), int(last, 16), fields]
	def _parseFile(self, path: str, legacyRanges: bool) -> None:
		print(f'Parsing [{path}]...')
		
		# open the file for reading and iterate over its lines
		with open(path, 'r', encoding='utf-8') as file:
			legacyState = None
			for line in file:
				# parse the line
				parsed = self._parseLine(line)
				if parsed is None:
					continue
				missing, begin, last, fields = parsed

				# check if a legacy range has been started
				if legacyState is not None:
					if len(fields) == 0 or ', Last>' not in fields[0] or fields[0][:-7] != legacyState[1] or fields[1:] != legacyState[2:]:
						raise RuntimeError(f'Legacy range not closed properly [{begin:06x}]')
					begin = legacyState[0]
					legacyState = None
				elif legacyRanges and ', First>' in fields[0]:
					legacyState = [begin, fields[0][:-8]] + fields[1:]
					continue

				# check if the line can be ignored, because its empty (i.e. only a comment)
				if len(fields) < 1:
					continue

				# write the value to the output
				self._parsed.append((begin, last, missing, fields))
			if legacyState is not None:
				raise RuntimeError(f'Half-open legacy state encountered [{legacyState[0]:06x}]')
	def __init__(self, path: str, legacyRanges: bool) -> None:
		self._parsed: list[tuple[int, int, bool, list[str]]] = []
		self._parseFile(path, legacyRanges)
	def filter(self, relevantFields: int, assignValue) -> list[Range]:
		ranges: list[Range] = []

		# iterate over the parsed lines and match them against the callback
		for (begin, last, _, fields) in self._parsed:
			# check if the line can be ignored
			if len(fields) < relevantFields:
				continue

			# fetch the assigned value and check if the entry is to be ignored
			value = assignValue(fields)
			if value is not None:
				ranges.append(Range(begin, last, value))

		# sanitize and cleanup the found ranges
		return Ranges.fromRawList(ranges)
	def extractAll(self, fieldCount: int, relevantField: int, valueMap: map) -> tuple[list[Range], int]:
		default, ranges = None, []

		# iterate over the parsed lines and validate them and extract the range-objects
		for (begin, last, missing, fields) in self._parsed:
			# check if the line can be ignored
			if len(fields) < fieldCount:
				raise RuntimeError(f'Too few fields in line [{begin:06x} - {last:06x}]')
				continue
			field = fields[relevantField].lower()

			# check if the value is well defined
			if field not in valueMap:
				raise RuntimeError(f'Unknown field encountered [{field}]')
			valIndex = valueMap[field]

			# check if this is the default value
			if missing:
				if default != None:
					raise RuntimeError('Multiple default values encountered')
				default = valIndex
			
			# add the range to the list
			else:
				ranges.append(Range(begin, last, valIndex))

		# check if a default value has been found
		if default is None:
			raise RuntimeError(f'Default value has not been found')

		# sanitize and organize the ranges
		return Ranges.fromRawList(ranges), default



# download all relevant files from the latest release of the ucd (unicode character database: https://www.unicode.org/Public/UCD/latest)
def DownloadUCDFiles(refreshFiles: bool) -> None:
	baseURL = 'https://www.unicode.org/Public/UCD/latest'
	files = {
		'UnicodeData.txt': 'ucd/UnicodeData.txt', 
		'PropList.txt': 'ucd/PropList.txt', 
		'DerivedCoreProperties.txt': 'ucd/DerivedCoreProperties.txt'
	}
	dirPath = './generated/ucd'

	# check if the directory needs to be created
	if not os.path.isdir(dirPath):
		os.mkdir(dirPath)

	# download all of the files (only if they should either be refreshed, or do not exist yet)
	for file in files:
		url, path = f'{baseURL}/{files[file]}', f'{dirPath}/{file}'
		if not refreshFiles and os.path.isfile(path):
			print(f'skipping [{path}] as the file already exists (use --refresh to enforce a new download)')
			continue
		print(f'downloading [{url}] to [{path}]...')
		urllib.request.urlretrieve(url, path)





# indirection optimization possible? i.e. buf1[buf0[data >> 4] ...]
# expand all paths and create cost estimations and then pick implementation based on costs





# TestAscii, TestAlpha, GetRadix, GetDigit, TestWhiteSpace, TestControl, TestLetter, GetPrintable, GetCase, GetCategory
def MakeCodepointQuery():
	# parse the relevant files
	unicodeData = ParsedFile('generated/ucd/UnicodeData.txt', True)
	derivedProperties = ParsedFile('generated/ucd/DerivedCoreProperties.txt', False)
	propList = ParsedFile('generated/ucd/PropList.txt', False)

	# write all lookup functions to the file
	with GeneratedFile('generated/unicode-cp-query.h', True) as file:
		# write the unicode-test to the file
		unicodeRanges = Ranges.difference([Range(0, 0x10ffff, 1)], unicodeData.filter(2, lambda fs: None if fs[1] != 'Cs' else 1))
		_gen: CodeGen = CodeGen(file, 'Unicode', 'Automatically generated from: Unicode General_Category is not cs (i.e. surrogate pairs) smaller than/equal to 0x10ffff')
		_gen.intFunction('TestUnicode', LookupType.boolType(), unicodeRanges)

		# write the assigned-test to the file
		assignedRanges = unicodeData.filter(2, lambda fs: None if fs[1] in ['Cs', 'Co', 'Cn'] else 1)
		_gen: CodeGen = CodeGen(file, 'Assigned', 'Automatically generated from: Unicode General_Category is not Cn, Cs, Co (i.e. not assigned, surrogate pairs, private use)')
		_gen.intFunction('TestAssigned', LookupType.boolType(), assignedRanges)

		# write the ascii-test to the file
		asciiRanges = [Range(0, 0x7f, 1)]
		_gen: CodeGen = CodeGen(file, 'Ascii', 'Automatically generated from: [cp <= 0x7f]')
		_gen.intFunction('TestAscii', LookupType.boolType(), asciiRanges)
		
		# write the alpha-test to the file
		alphaRanges = Ranges.fromRawList([Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)])
		_gen: CodeGen = CodeGen(file, 'Alpha', 'Automatically generated from: [a-zA-Z]')
		_gen.intFunction('TestAlpha', LookupType.boolType(), alphaRanges)

		# write the radix-getter to the file
		radixRanges = Ranges.fromRawList([Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)])
		_gen: CodeGen = CodeGen(file, 'Radix', 'Automatically generated from: [0-9a-zA-Z] mapped to [0-35] and rest to 0xff')
		_gen.intFunction('GetRadix', LookupType.intType(0xff, 'uint8_t'), radixRanges)
	
		# write the digit-getter to the file (https://www.unicode.org/reports/tr44/#Numeric_Type)
		digitRanges = unicodeData.filter(8, lambda fs: int(fs[5]) if fs[5] != '' and fs[5] in '0123456789' else None)
		digitRanges = Ranges.merge(digitRanges, unicodeData.filter(8, lambda fs: 0xf0 if fs[6] != '' else None), True)
		digitRanges = Ranges.merge(digitRanges, unicodeData.filter(8, lambda fs: 0xf1 if fs[7] != '' else None), True)
		_gen: CodeGen = CodeGen(file, 'Digit', 'Automatically generated from: Unicode: Numeric_Type=Decimal: [0-9]; Numeric_Type=Digit: [0xf0]; Numeric_Type=Numeric: [0xf1]; rest [0xff]')
		_gen.intFunction('GetDigit', LookupType.intType(0xff, 'uint8_t'), digitRanges)

		# write the whitespace-test to the file (https://www.unicode.org/reports/tr44/#White_Space)
		whiteSpaceRanges = propList.filter(1, lambda fs: None if fs[0] != 'White_Space' else 1)
		_gen: CodeGen = CodeGen(file, 'WhiteSpace', 'Automatically generated from: Unicode White_Space property')
		_gen.intFunction('TestWhiteSpace', LookupType.boolType(), whiteSpaceRanges)

		# write the control-test to the file (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
		controlRanges = unicodeData.filter(2, lambda fs: None if fs[1] != 'Cc' else 1)
		_gen: CodeGen = CodeGen(file, 'Control', 'Automatically generated from: Unicode General_Category is cc (i.e. C0, C1)')
		_gen.intFunction('TestControl', LookupType.boolType(), controlRanges)

		# write the letter-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic)
		letterRanges = derivedProperties.filter(1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		_gen: CodeGen = CodeGen(file, 'Letter', 'Automatically generated from: Unicode derived property Alphabetic')
		_gen.intFunction('TestLetter', LookupType.boolType(), letterRanges)

		# write the alpha-num-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic + https://www.unicode.org/reports/tr44/#Numeric_Type)
		alnumRanges = derivedProperties.filter(1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		alnumRanges = Ranges.union(alnumRanges, unicodeData.filter(8, lambda fs: 1 if fs[7] != '' else None))
		_gen: CodeGen = CodeGen(file, 'AlNum', 'Automatically generated from: Unicode derived property Alphabetic or Numeric_Type=Decimal,Digit,Numeric')
		_gen.intFunction('TestAlNum', LookupType.boolType(), alnumRanges)
		
		# write the printable-enum to the file (https://en.wikipedia.org/wiki/Graphic_character)
		printableFilterMap = { 
			'Lu': 1, 'Ll': 1, 'Lt': 1, 'Lm': 1, 'Lo': 1, 'Mn': 1, 'Mc': 1, 'Me': 1, 'Nd': 1, 'Nl': 1, 'No': 1,
			'Pc': 1, 'Pd': 1, 'Ps': 1, 'Pe': 1, 'Pi': 1, 'Pf': 1, 'Po': 1, 'Sm': 1, 'Sc': 1, 'Sk': 1, 'So': 1,
			'Zs': 2 
		}
		printableRanges = unicodeData.filter(2, lambda fs: printableFilterMap[fs[1]] if fs[1] in printableFilterMap else None)
		_enum: LookupType = LookupType.enumType('PrintableType', 'none', ['none', 'printable', 'printSpace'])
		_gen: CodeGen = CodeGen(file, 'Printable', 'Automatically generated from: Unicode General_Category is L*,M*,N*,P*,S* or optionally Zs')
		_gen.addEnum(_enum)
		_gen.intFunction('GetPrintable', _enum, printableRanges)

		# write the cased-enum to the file (https://www.unicode.org/reports/tr44/#Cased)
		caseFilterMap = { 'Lowercase': 1, 'Uppercase': 2 }
		caseRanges = derivedProperties.filter(1, lambda fs: caseFilterMap[fs[0]] if fs[0] in caseFilterMap else None)
		caseRanges = Ranges.union(caseRanges, unicodeData.filter(2, lambda fs: 3 if fs[1] == 'Lt' else None))
		_enum: LookupType = LookupType.enumType('CaseType', 'none', ['none', 'lowerCase', 'upperCase', 'titleCase'])
		_gen: CodeGen = CodeGen(file, 'Case', 'Automatically generated from: Unicode derived property Lowercase, Uppercase or General_Category Lt')
		_gen.addEnum(_enum)
		_gen.intFunction('GetCase', _enum, caseRanges)

		# write the category-enum to the file (https://www.unicode.org/reports/tr44/#GC_Values_Table)
		categoryEnumMap = {
			'Lu': 0, 'Ll': 1, 'Lt': 2, 'Lm': 3, 'Lo': 4, 'Mn': 5, 'Mc': 6, 'Me': 7, 'Nd': 8, 'Nl': 9, 'No': 10,
			'Pc': 11, 'Pd': 12, 'Ps': 13, 'Pe': 14, 'Pi': 15, 'Pf': 16, 'Po': 17, 'Sm': 18, 'Sc': 19, 'Sk': 20, 'So': 21,
			'Zs': 22, 'Zl': 23, 'Zp': 24, 'Cc': 25, 'Cf': 26, 'Cs': 27, 'Co': 28, 'Cn': 29
		}
		categoryRanges = unicodeData.filter(2, lambda fs: categoryEnumMap[fs[1]] if fs[1] in categoryEnumMap else None)
		_enum: LookupType = LookupType.enumType('CategoryType', 'cn', ['lu', 'll', 'lt', 'lm', 'lo', 'mn', 'mc', 'me', 'nd', 'nl', 'no', 'pc', 'pd', 'ps', 'pe', 'pi', 'pf', 'po', 'sm', 'sc', 'sk', 'so', 'zs', 'zl', 'zp', 'cc', 'cf', 'cs', 'co', 'cn' ])
		_gen: CodeGen = CodeGen(file, 'Category', 'Automatically generated from: Unicode General_Category')
		_gen.addEnum(_enum)
		_gen.intFunction('GetCategory', _enum, categoryRanges)

def MakeCodepointMaps():
	# parse the relevant files
	unicodeData = ParsedFile('generated/ucd/UnicodeData.txt', True)
	
	# write all maps functions to the file
	with GeneratedFile('generated/unicode-cp-maps.h', True) as file:
		# write the uppercase-mapping to the file
		upperRanges = unicodeData.filter(12, lambda fs: int(fs[11], 16) if fs[11] != '' else None)
		upperRanges = Ranges.translate(upperRanges, lambda i, v: v[0] - i)
		_gen: CodeGen = CodeGen(file, 'Uppercase', '...')
		_gen.listFunction('MapUppercase', LookupType.intType(0, 'int32_t'), upperRanges)

		
		pass


# ToLower (?)
# ToUpper (?)

def MakeGraphemeTypeMapping():
	# parse the relevant files
	graphemeBreakProperty = RawParseFile('ucd/GraphemeBreakProperty.txt', False)
	emojiData = RawParseFile('ucd/emoji-data.txt', False)

	graphemeEnumName, graphemeEnumMap = 'GraphemeType', {
		'other': 0, 'prepend': 1, 'cr': 2, 'lf': 3, 
		'control': 4, 'extend': 5, 'regional_indicator': 6, 'spacingmark': 7, 
		'l': 8, 'v': 9, 't': 10, 'lv': 11, 'lvt': 12, 'zwj': 13, 'extended_pictographic': 14
	}

	# parse the GraphemeBreakProperty.txt file to extract the grapheme-properties
	ranges, defValue = ExtractProperties(graphemeBreakProperty, 0, graphemeEnumMap, True, True)
	if defValue != graphemeEnumMap['other']:
		raise RuntimeError('Default grapheme-value is expected to be [other]')
	
	# parse the emoji-data.txt file to extract the Extended_Pictographic property
	ranges += ExtractProperties(emojiData, 0, { 'extended_pictographic': graphemeEnumMap['extended_pictographic'] }, False, False)[0]

	# open the output file and generate the code into it
	with open('grapheme-type.h', 'w', encoding='ascii') as file:
		BeginFile(file)
		
		# write the grapheme-enum to the file
		WriteEnumString(file, graphemeEnumName, graphemeEnumMap)
		WriteLookup(file, ranges, Config('LookupGraphemeType', 'Grapheme', 'gen::Grapheme', f'gen::{graphemeEnumName}', f'static_cast<gen::{graphemeEnumName}>', InvertMap(f'gen::{graphemeEnumName}::', graphemeEnumMap), 0))
		
		EndFile(file)



# check if the files need to be downloaded
DownloadUCDFiles('--refresh' in sys.argv)

MakeCodepointQuery()
MakeCodepointMaps()
# MakeGraphemeTypeMapping()
