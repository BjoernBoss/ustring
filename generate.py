import urllib.request
import os
import sys
import datetime

# ranges are lists of range-objects, which must be sorted and must not overlap
# ranges map [first-last] to an integer [>= 0]
# invariant for ranges: (first >= 0) and (first <= last) and (last <= 2**32 - 1)

class Range:
	RangeFirst: int = 0
	RangeLast: int = 2**32 - 1

	def __init__(self, first: int, last: int, value: int) -> None:
		if first < Range.RangeFirst or last > Range.RangeLast or first > last:
			raise RuntimeError('Malformed range encountered')
		if value < 0:
			raise RuntimeError('Malformed value encountered')
		self.first = first
		self.last = last
		self.value = value
	def __str__(self) -> str:
		return f'[{self.first:05x}-{self.last:05x}/{self.span()}]:{self.value}'
	def __repr__(self) -> str:
		return self.__str__()
	def merge(self, other: 'Range') -> 'Range':
		if self.value != other.value:
			raise RuntimeError('Cannot merge ranges of different value')
		return Range(min(self.first, other.first), max(self.last, other.last), self.value)
	def split(self, size: int) -> tuple['Range', 'Range']:
		if self.first + size > self.last:
			raise RuntimeError(f'Can only split a range within itself [{self.span()}/{size}]')
		left = Range(self.first, self.first + size - 1, self.value)
		right = Range(self.first + size, self.last, self.value)
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
	def Bool(defValue: bool) -> 'LookupType':
		out = LookupType()
		out._kind = 'bool'
		out._typeName = 'bool'
		out._default = 1 if defValue else 0
		return out
	@staticmethod
	def Int(intType: str, defValue: int) -> 'LookupType':
		out = LookupType()
		out._kind = 'int'
		out._typeName = intType
		out._default = defValue
		return out
	@staticmethod
	def Enum(name: str, values: list[str], defValue: str) -> 'LookupType':
		out = LookupType()
		out._kind = 'enum'
		out._typeName = f'gen::{name}'
		out._values = values
		out._default = values.index(defValue)
		if len(out._values) == 0:
			raise RuntimeError(f'Enum [{name}] must not be empty')
		return out
	
	def isBoolean(self) -> bool:
		return (self._kind == 'bool')
	def values(self, valIfInfinite):
		if self._kind == 'bool':
			return 2
		if self._kind == 'int':
			return valIfInfinite
		if self._kind == 'enum':
			return len(self._values)
	def typeName(self) -> str:
		return self._typeName
	def defValue(self) -> int:
		if self._kind == 'bool':
			return self._default
		if self._kind == 'int':
			return self._default
		if self._kind == 'enum':
			return self._default
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def staticLookup(self, index) -> str:
		if self._kind == 'bool':
			return 'true' if index > 0 else 'false'
		if self._kind == 'int':
			return str(index)
		if self._kind == 'enum':
			return f'{self._typeName}::{self._values[index]}'
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
	def dynLookup(self, varName) -> str:
		if self._kind == 'bool':
			return f'({varName} != 0)'
		if self._kind == 'int':
			return f'{self._typeName}({varName})'
		if self._kind == 'enum':
			return f'static_cast<{self._typeName}>({varName})'
		raise RuntimeError(f'Unknown kind [{self._kind}] encountered')

class GeneratedFile:
	def __init__(self, path: str, addBinSearch: bool) -> None:
		self._path = path
		self._file = None
		self._addBinSearch = addBinSearch
		self._hadFirstBlock = False
		self._atStartOfLine = False
		self._indented = False
		self._blockName = None
		self._bufferIndex = 0
	def __enter__(self) -> 'GeneratedFile':
		self._file = open(self._path, mode='w', encoding='ascii')
		self._atStartOfLine = True

		# write the file header
		self.writeln('#pragma once')
		self.writeln('')
		self.writeln('#include <cinttypes>')
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
	def beginBlock(self, msg: str, name: str) -> None:
		self._blockName = name
		self._bufferIndex = 0

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
	def writeEnum(self, name: str, map: list[str]) -> None:
		# extract the enum-type to be used
		enumType = 'uint8_t'
		if len(map) > 2**16:
			enumType = 'uint32_t'
		elif len(map) > 2**8:
			enumType = 'uint16_t'

		# write the enum out
		self.write(f'enum class {name} : {enumType} {{\n\t')
		self.writeln(",\n\t".join(map))
		self.writeln('};')
	def writeBuffer(self, data: list[int]) -> str:
		# construct the buffer-name
		name = f'{self._blockName}Buf{self._bufferIndex}'
		self._bufferIndex += 1

		# fetch the buffer-type to be used
		typeString, largest = 'uint32_t', max(data)
		if largest < 256:
			typeString = 'uint8_t'
		elif largest < 65536:
			typeString = 'uint16_t'

		# check if the values should be written as hex (if more than a quarter of the values are larger than 256)
		printAsHex = (sum(1 for d in data if d >= 256) * 4 > len(data))

		# slightly align the count to get a closer resembles of rectangles
		valsPerLine = 24 if printAsHex else 32
		estimatedLines = (len(data) + valsPerLine - 1) // valsPerLine
		valsPerLine = (len(data) + estimatedLines - 1) // estimatedLines
	
		# write the header and the data out
		self.write(f'static constexpr {typeString} {name}[{len(data)}] = {{\n\t')
		for i in range(len(data)):
			if i > 0:
				self.write(f',{"\n\t" if (i % valsPerLine) == 0 else ""}')
			self.write(f' 0x{data[i]:04x}' if printAsHex else f'{data[i]:3}')

		# close the buffer-string and return the access name
		self.writeln('\n};')
		return f'gen::{name}'
	def binSearch(self, data: str) -> str:
		return f'gen::BinarySearch(cp, {data})'

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
	def multiLines(string: str, excludeTrailing: bool) -> bool:
		if excludeTrailing:
			return (string[:-1].find('\n') >= 0)
		return (string.find('\n') >= 0)

class Ranges:
	@staticmethod
	def _appOrMerge(out: list[Range], other: Range) -> None:
		if len(out) > 0 and (out[-1].overlap(other) or (out[-1].neighbors(other) and out[-1].value == other.value)):
			out[-1] = out[-1].merge(other)
		else:
			out.append(other)
	@staticmethod
	def _setIteration(a: list[Range], b: list[Range], binaryOp: bool, fn) -> list[Range]:
		if binaryOp and (any(_a.value != 1 for _a in a) or any(_b.value != 1 for _b in b)):
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
			if first < aFirst and last >= aFirst:
				last = aFirst - 1
			if first < bFirst and last >= bFirst:
				last = bFirst - 1
			
			# invoke the callback and add the next range to the output
			val = fn(aNext.value if aFirst <= first else None, bNext.value if bFirst <= first else None)
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
	def _anyUnionOperationFavFirst(a: int|None, b: int|None) -> int|None:
		if a is None:
			return b
		return a
	@staticmethod
	def _unionOperation(a: int|None, b: int|None) -> int|None:
		if a is None:
			return b
		if b is None:
			return a
		
		# perform merge to trigger an exception on invalid merges
		return Range(0, 0, a).merge(Range(0, 0, b)).value
	@staticmethod
	def _intersectOperation(a: int|None, b: int|None) -> int|None:
		if a is None or b is None:
			return None
		return 1
	@staticmethod
	def _differenceOperation(a: int|None, b: int|None) -> int|None:
		if b is None:
			return a
		return None

	@staticmethod
	def fromUnordered(ranges: list[Range]) -> list[Range]:
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
			if i > 0 and ranges[i].first < ranges[i - 1].first:
				raise RuntimeError('Order of ranges violation encountered')
			if type(ranges[i].value) != int or ranges[i].value < 0:
				raise RuntimeError('Invalid value encountered for range encountered')
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
	def reduce(ranges: list[Range]) -> list[Range]:
		out: list[Range] = []
		for i in range(len(ranges)):
			if len(out) > 0 and (out[-1].neighbors(ranges[i]) and out[-1].value == ranges[i].value):
				out[-1] = out[-1].merge(ranges[i])
			else:
				out.append(ranges[i])
		return out
	@staticmethod
	def fill(ranges: list[Range], leftEdge: int, rightEdge: int, fillValue) -> list[Range]:
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
	def filter(ranges: list[Range], drop: int) -> list[Range]:
		out: list[Range] = []
		for i in range(len(ranges)):
			if ranges[i].value != drop:
				out.append(ranges[i])
		return out
	@staticmethod
	def cluster(ranges: list[Range], clusters: list[Range]) -> list[Range]:
		if len(clusters) == 0:
			return ranges
		out = []

		# iterate over the ranges and write them to the output or cluster them together, if a cluster matches
		rIndex, cIndex = 0, 0
		while rIndex < len(ranges) and cIndex < len(clusters):
			if rIndex < clusters[cIndex].first:
				out.append(ranges[rIndex])
				rIndex += 1
				continue
			cluster: Range = clusters[cIndex]
			out.append(Range(ranges[cluster.first].first, ranges[cluster.last].last, cluster.value))
			rIndex = cluster.last + 1
			cIndex += 1
		
		# append the remaining ranges
		if rIndex < len(ranges):
			out += ranges[rIndex:]
		return out

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
			if a[i].value != 1:
				raise RuntimeError('Complement operation only defined for binary ranges')
			if a[i].first > lastEnd + 1:
				out.append(Range(lastEnd + 1, a[i].first - 1, 1))
			lastEnd = a[i].last

		# add the final inversion to the 2**32
		if lastEnd + 1 < Range.RangeLast:
			out.append(Range(lastEnd + 1, Range.RangeLast, 1))
		return out

def DensityClustering(ranges: list[Range], startIndex: int) -> list[Range]:
	thresholdDensity, thresholdChars = 1.0 / 2.0, 8

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
		if bestDensity < thresholdDensity:
			break
		l, r = clusters[best], clusters[best + 1]
		clusters[best] = (l[0], r[1], l[2] + r[2], l[3] + r[3])
		clusters = clusters[:best + 1] + clusters[best + 2:]

	# remove all clusters, which either did not reach the density or chars threshold
	index = 0
	while index < len(clusters):
		if clusters[index][3] >= thresholdChars and (clusters[index][2] / clusters[index][3]) >= thresholdDensity:
			index += 1
		else:
			clusters = clusters[:index] + clusters[index + 1:]

	# transform the clusters to the required output format (will be sorted as the algorithm does not reorder them)
	return [Range(clusters[i][0], clusters[i][1], startIndex + i) for i in range(len(clusters))]
def WriteLookupCodeAndBuffer(file: GeneratedFile, farReducedRanges: list[Range], boundLeft: int, boundRight: int, varName: str, forceGeneration: bool, usingChars: bool, lookupType: LookupType) -> str|None:
	# check if it is a simple lookup, as a code/buffer lookup otherwise does not make sense (simple: low spread/few ranges; first fill after the check, as the range-count will otherwise be distorted)
	rangeCoveredChars, actualRangeChars = (farReducedRanges[-1].last - farReducedRanges[0].first + 1), sum(r.span() for r in farReducedRanges)
	if not forceGeneration and len(farReducedRanges) >= 24 and (rangeCoveredChars > 1.75 * actualRangeChars or rangeCoveredChars > 3.0 * len(farReducedRanges)):
		return None
	ranges: list[Range] = Ranges.fill(farReducedRanges, boundLeft, boundRight, lookupType.defValue())

	# perform density-clustering to detect if buffers need to be created
	firstCluster = max(v.value for v in ranges) + 1
	clusters = DensityClustering(ranges, firstCluster)
	
	# write the cluster-buffers out and merge the ranges together
	clusterBufferName = []
	for i in range(len(clusters)):
		data = [ranges[r].value for r in range(clusters[i].first, clusters[i].last + 1) for _ in range(ranges[r].span())]
		clusterBufferName.append(file.writeBuffer(data))
	ranges = Ranges.cluster(ranges, clusters)

	# statup the active state of range-values to range-count and char-count
	state = {}
	for i in range(len(ranges)):
		if ranges[i].value not in state:
			state[ranges[i].value] = [1, ranges[i].span()]
		else:
			state[ranges[i].value][0] += 1
			state[ranges[i].value][1] += ranges[i].span()

	# iterate until all ranges have been processed/inserted
	lookupCode = ''
	while len(ranges) > 0:
		# look for the value-index with the largest number of characters, which consists of the fewest number of ranges
		bestValue, bestRanges, bestChars = 0, 0, 0
		for k, (ra, ch) in state.items():
			if (bestRanges == 0 or bestRanges > ra or (bestRanges == ra and bestChars < ch)):
				bestValue, bestRanges, bestChars = k, ra, ch
		state.pop(bestValue)

		# extract all ranges selected by the best value-index and merge the remaining ranges (mis-use the value as character-count within the range)
		practical: list[Range] = []
		index = 0
		while index < len(ranges):
			if ranges[index].value != bestValue:
				index += 1
				continue

			# extract the range of ranges to be removed
			last, chars = index + 1, ranges[index].span()
			while last < len(ranges) and ranges[last].value == bestValue:
				chars += ranges[last].span()
				last += 1
			
			# add the new practical range and remove the range from the open set of ranges
			practical.append(Range(ranges[index].first, ranges[last - 1].last, chars))
			ranges = ranges[:index] + ranges[last:]

			# check if the neighboring ranges can now be considered neighbors (cannot be of the selected type)
			# in which case the state must be updated to keep track of the practical number of ranges
			if index > 0 and index < len(ranges) and ranges[index - 1].value == ranges[index].value:
				state[ranges[index].value][0] -= 1

		# check which side of the outer bounds of the actual ranges need to be checked
		checkLeft, checkRight = (len(ranges) > 0 and ranges[0].first < practical[0].first), (len(ranges) > 0 and ranges[-1].last > practical[-1].last)
		
		# check if this is a cluster-entry, in which case the buffer needs to be dereferenced (there can only be one practical range per cluster-entry)
		if bestValue >= firstCluster:
			first, last = practical[0].first, practical[0].last

			# check which bounds need to be checked (all other value variations will already have been checked and cleared)
			if checkLeft or checkRight:
				lookupCode += 'if ('
				lookupCode += f'{varName} >= {StrHelp.value(first, usingChars)}' if checkLeft else ''
				lookupCode += ' && ' if (checkLeft and checkRight) else ''
				lookupCode += f'{varName} <= {StrHelp.value(last, usingChars)}' if checkRight else ''
				lookupCode += f')\n\treturn '
			else:
				lookupCode += f'return '

			# add the dereferencing
			lookupKey = f'{clusterBufferName[bestValue - firstCluster]}[{varName}'
			if first > 0:
				lookupKey += f' - {first}'
			lookupKey += ']'
			lookupCode += lookupType.dynLookup(lookupKey) + ';\n'
			continue

		# check if these are the last ranges, in which case no if-statement is necessary
		if len(ranges) == 0:
			lookupCode += f'return {lookupType.staticLookup(bestValue)};\n'
			continue

		# write the if-statement out and potentially break it, in case it could become an overlong row
		index = 0
		while index < len(practical):
			count = len(practical) - index
			if count >= 8:
				count = 4
			elif count > 4:
				count = (count + 1) // 2
		
			# add the next if-statement
			lookupCode += f'if ('

			# iterate over the ranges to be tested and add them to the condition (check greater/less equal before equality, to ensure open sides are handled properly)
			for i in range(index, index + count):
				if i > index:
					lookupCode += ' || '
				
				first, last = practical[i].first, practical[i].last
				if not checkLeft and i == 0:
					lookupCode += f'{varName} <= {StrHelp.value(last, usingChars)}'
				elif not checkRight and i + 1 == len(practical):
					lookupCode += f'{varName} >= {StrHelp.value(first, usingChars)}'
				elif practical[i].value == 1:
					lookupCode += f'{varName} == {StrHelp.value(first, usingChars)}'
				elif practical[i].value == 2:
					lookupCode += f'{varName} == {StrHelp.value(first, usingChars)} || {varName} == {StrHelp.value(last, usingChars)}'
				elif count > 1:
					lookupCode += f'({varName} >= {StrHelp.value(first, usingChars)} && {varName} <= {StrHelp.value(last, usingChars)})'
				else:
					lookupCode += f'{varName} >= {StrHelp.value(first, usingChars)} && {varName} <= {StrHelp.value(last, usingChars)}'
			index += count

			# add the end of the if-statement and the return of the value
			lookupCode += f')\n\treturn {lookupType.staticLookup(bestValue)};\n'
	return lookupCode
def WriteLookupBinarySearch(file: GeneratedFile, farReducedRanges: list[Range], lookupType: LookupType) -> str:
	# update the ranges to adhere to the size-constraints of 16 bits (larger by one, as values are stored decreased by one)
	ranges: list[Range] = Ranges.limitSize(farReducedRanges, 0x10000)
	
	# check if there are more than two values, in which case a value-map is required
	lookupTypeCode, lookupTypeBuffer = None, None
	if not lookupType.isBoolean():
		tempRanges = Ranges.reduce([Range(i, i, ranges[i].value) for i in range(len(ranges))])

		# check if a code/buffer lookup can be used and otherwise create the buffer
		lookupTypeCode = WriteLookupCodeAndBuffer(file, tempRanges, 0, len(ranges) - 1, 'index', False, False, lookupType)
		if lookupTypeCode is None:
			lookupTypeBuffer = file.writeBuffer([v.value for v in ranges])

	# write the range-sizes and rage-firsts, (store the size smaller by 1 to allow 65536 to be written)
	sizeBuffer = file.writeBuffer([r.span() - 1 for r in ranges])
	firstBuffer = file.writeBuffer([r.first for r in ranges])
	
	# setup the code to perform the binary-search (check cp greater than size, as size is reduced by one)
	findValue = 'size_t index = ' + file.binSearch(firstBuffer) + ';\n'
	if lookupType.isBoolean():
		return findValue + f'return (cp - {firstBuffer}[index] {"<=" if lookupType.defValue() == 0 else ">"} {sizeBuffer}[index]);\n'
	findValue += f'if (cp - {firstBuffer}[index] > {sizeBuffer}[index])\n'
	findValue += f'\treturn {lookupType.staticLookup(lookupType.defValue())};\n'
	
	# either add the call to the type-lookup or add the array index/return the static value
	if lookupTypeBuffer is None:
		findValue += lookupTypeCode
	else:
		findValue += f'return {lookupType.dynLookup('index')};\n'
	return findValue
def WriteLookupBounded(file: GeneratedFile, farReducedRanges: list[Range], boundLeft: int, boundRight: int, lookupType: LookupType) -> str:
	# check if this lookup can be done by a simple code/buffer lookup (only force if ascii-only)
	lookupCode = WriteLookupCodeAndBuffer(file, farReducedRanges, boundLeft, boundRight, 'cp', farReducedRanges[-1].last < 0x80, True, lookupType)
	if lookupCode is not None:
		return lookupCode
	
	# break the ranges into [ascii][0x80 - 0xffff][0x10000 - ...]
	asciiRanges, restRanges = Ranges.split(farReducedRanges, 0x80)
	lowerRanges, upperRanges = Ranges.split(restRanges, 0x10000)

	# setup the lookups for the ranges
	asciiLookup, lowerLookup, largeLookup = None, None, None
	if len(asciiRanges) > 0:
		asciiLookup = WriteLookupCodeAndBuffer(file, asciiRanges, 0x00, 0x7f, 'cp', True, True, lookupType)
	if len(lowerRanges) > 0 and len(upperRanges) > 0:
		lowerLookup = WriteLookupBounded(file, lowerRanges, 0x80 if len(asciiRanges) > 0 else 0x00, 0x10000, lookupType)
		largeLookup = WriteLookupBounded(file, upperRanges, 0x10000, boundRight, lookupType)
	else:
		largeLookup = WriteLookupBinarySearch(file, lowerRanges if len(lowerRanges) > 0 else upperRanges, lookupType)

	# generate the glue code for the ranges
	lookupCode = ''
	if asciiLookup is not None:
		lookupCode += 'if (cp < 0x80)'
		if StrHelp.multiLines(asciiLookup, True):
			lookupCode += ' {\n' + StrHelp.indent(asciiLookup) + '}\n'
		else:
			lookupCode += '\n' + StrHelp.indent(asciiLookup)
	if lowerLookup is not None:
		lookupCode += 'if (cp < 0x10000)'
		if StrHelp.multiLines(lowerLookup, True):
			lookupCode += ' {\n' + StrHelp.indent(lowerLookup) + '}\n'
		else:
			lookupCode += '\n' + StrHelp.indent(lowerLookup)
	return lookupCode + largeLookup
def WriteLookupFunction(file: GeneratedFile, fnName: str, ranges: list[Range], lookupType: LookupType) -> None:
	print(f'Creating {fnName}...')

	# validate the ranges and remove all default-ranges
	Ranges.wellFormed(ranges)
	farReducedRanges: list[Range] = Ranges.reduce(Ranges.filter(ranges, lookupType.defValue()))

	# create the actual code for the lookup
	if len(farReducedRanges) > 0:
		lookupCode = WriteLookupBounded(file, farReducedRanges, Range.RangeFirst, Range.RangeLast, lookupType)
	else:
		lookupCode = f'\treturn {lookupType.staticLookup(lookupType.defValue())};\n'

	# generate the actual function code (i.e. the actual function)
	file.writeln(f'inline constexpr {lookupType.typeName()} {fnName}(char32_t cp) {{')
	file.write(StrHelp.indent(lookupCode))
	file.writeln('}')


def ParseLine(line):
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
def RawParseFile(path: str, legacyRanges: bool) -> list[tuple[int, int, bool, list[str]]]:
	# list of (begin, last, missing, fields)
	out = []

	# open the file for reading and iterate over its lines
	print(f'Parsing [{path}]...')
	with open(path, 'r', encoding='utf-8') as file:
		legacyState = None
		for line in file:
			# parse the line
			parsed = ParseLine(line)
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
			out.append((begin, last, missing, fields))
		if legacyState is not None:
			raise RuntimeError(f'Half-open legacy state encountered [{legacyState[0]:06x}]')
	return out
def ExtractAllProperties(parsed, fieldCount: int, relevantField: int, valueMap: map) -> tuple[list[Range], int]:
	default, ranges = None, []

	# iterate over the parsed lines and validate them and extract the range-objects
	for (begin, last, missing, fields) in parsed:
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
	return Ranges.fromUnordered(ranges), default
def FilterProperties(parsed, relevantFields: int, assignValue) -> list[Range]:
	ranges: list[Range] = []

	# iterate over the parsed lines and match them against the callback
	for (begin, last, _, fields) in parsed:
		# check if the line can be ignored
		if len(fields) < relevantFields:
			continue

		# fetch the assigned value and check if the entry is to be ignored
		value = assignValue(fields)
		if value is not None:
			ranges.append(Range(begin, last, value))

	# sanitize and cleanup the found ranges
	return Ranges.fromUnordered(ranges)



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



# TestAscii, TestAlpha, GetRadix, GetDigit, TestWhiteSpace, TestControl, TestLetter, GetPrintable, GetCase, GetCategory
def MakeCodepointQuery():
	# parse the relevant files
	unicodeData = RawParseFile('generated/ucd/UnicodeData.txt', True)
	derivedProperties = RawParseFile('generated/ucd/DerivedCoreProperties.txt', False)
	propList = RawParseFile('generated/ucd/PropList.txt', False)

	# write the state to the file
	with GeneratedFile('generated/unicode-cp-query.h', True) as file:
		# write the unicode-test to the file
		unicodeRanges = Ranges.difference([Range(0, 0x10ffff, 1)], FilterProperties(unicodeData, 2, lambda fs: None if fs[1] != 'Cs' else 1))
		file.beginBlock('Automatically generated from: Unicode General_Category is not cs (i.e. surrogate pairs) smaller than/equal to 0x10ffff', 'Unicode')
		WriteLookupFunction(file, 'TestUnicode', unicodeRanges, LookupType.Bool(False))

		# write the assigned-test to the file
		assignedRanges = FilterProperties(unicodeData, 2, lambda fs: None if fs[1] in ['Cs', 'Co', 'Cn'] else 1)
		file.beginBlock('Automatically generated from: Unicode General_Category is not Cn, Cs, Co (i.e. not assigned, surrogate pairs, private use)', 'Assigned')
		WriteLookupFunction(file, 'TestAssigned', assignedRanges, LookupType.Bool(False))
		
		# write the ascii-test to the file
		asciiRanges = [Range(0, 0x7f, 1)]
		file.beginBlock('Automatically generated from: [cp <= 0x7f]', 'Ascii')
		WriteLookupFunction(file, 'TestAscii', asciiRanges, LookupType.Bool(False))

		# write the alpha-test to the file
		alphaRanges = Ranges.fromUnordered([Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)])
		file.beginBlock('Automatically generated from: [a-zA-Z]', 'Alpha')
		WriteLookupFunction(file, 'TestAlpha', alphaRanges, LookupType.Bool(False))

		# write the radix-getter to the file
		radixRanges = Ranges.fromUnordered([Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)])
		file.beginBlock('Automatically generated from: [0-9a-zA-Z] mapped to [0-35] and rest to 0xff', 'Radix')
		WriteLookupFunction(file, 'GetRadix', radixRanges, LookupType.Int('uint8_t', 0xff))
	
		# write the digit-getter to the file (https://www.unicode.org/reports/tr44/#Numeric_Type)
		digitRanges = FilterProperties(unicodeData, 8, lambda fs: int(fs[5]) if fs[5] != '' and fs[5] in '0123456789' else None)
		digitRanges = Ranges.merge(digitRanges, FilterProperties(unicodeData, 8, lambda fs: 0xf0 if fs[6] != '' else None), True)
		digitRanges = Ranges.merge(digitRanges, FilterProperties(unicodeData, 8, lambda fs: 0xf1 if fs[7] != '' else None), True)
		file.beginBlock('Automatically generated from: Unicode: Numeric_Type=Decimal: [0-9]; Numeric_Type=Digit: [0xf0]; Numeric_Type=Numeric: [0xf1]; rest [0xff]', 'Digit')
		WriteLookupFunction(file, 'GetDigit', digitRanges, LookupType.Int('uint8_t', 0xff))

		# write the whitespace-test to the file (https://www.unicode.org/reports/tr44/#White_Space)
		whiteSpaceRanges = FilterProperties(propList, 1, lambda fs: None if fs[0] != 'White_Space' else 1)
		file.beginBlock('Automatically generated from: Unicode White_Space property', 'WhiteSpace')
		WriteLookupFunction(file, 'TestWhiteSpace', whiteSpaceRanges, LookupType.Bool(False))

		# write the control-test to the file (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
		controlRanges = FilterProperties(unicodeData, 2, lambda fs: None if fs[1] != 'Cc' else 1)
		file.beginBlock('Automatically generated from: Unicode General_Category is cc (i.e. C0, C1)', 'Control')
		WriteLookupFunction(file, 'TestControl', controlRanges, LookupType.Bool(False))

		# write the letter-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic)
		letterRanges = FilterProperties(derivedProperties, 1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		file.beginBlock('Automatically generated from: Unicode derived property Alphabetic', 'Letter')
		WriteLookupFunction(file, 'TestLetter', letterRanges, LookupType.Bool(False))

		# write the alpha-num-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic + https://www.unicode.org/reports/tr44/#Numeric_Type)
		alnumRanges = FilterProperties(derivedProperties, 1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		alnumRanges = Ranges.union(alnumRanges, FilterProperties(unicodeData, 8, lambda fs: 1 if fs[7] != '' else None))
		file.beginBlock('Automatically generated from: Unicode derived property Alphabetic or Numeric_Type=Decimal,Digit,Numeric', 'AlNum')
		WriteLookupFunction(file, 'TestAlNum', alnumRanges, LookupType.Bool(False))

		# write the printable-enum to the file (https://en.wikipedia.org/wiki/Graphic_character)
		printableFilterMap = { 
			'Lu': 1, 'Ll': 1, 'Lt': 1, 'Lm': 1, 'Lo': 1, 'Mn': 1, 'Mc': 1, 'Me': 1, 'Nd': 1, 'Nl': 1, 'No': 1,
			'Pc': 1, 'Pd': 1, 'Ps': 1, 'Pe': 1, 'Pi': 1, 'Pf': 1, 'Po': 1, 'Sm': 1, 'Sc': 1, 'Sk': 1, 'So': 1,
			'Zs': 2 
		}
		printableRanges = FilterProperties(unicodeData, 2, lambda fs: printableFilterMap[fs[1]] if fs[1] in printableFilterMap else None)
		file.beginBlock('Automatically generated from: Unicode General_Category is L*,M*,N*,P*,S* or optionally Zs', 'Printable')
		file.writeEnum('PrintableType', ['none', 'printable', 'printSpace'])
		WriteLookupFunction(file, 'GetPrintable', printableRanges, LookupType.Enum('PrintableType', ['none', 'printable', 'printSpace'], 'none'))
		
		# write the cased-enum to the file (https://www.unicode.org/reports/tr44/#Cased)
		caseFilterMap = { 'Lowercase': 1, 'Uppercase': 2 }
		caseRanges = FilterProperties(derivedProperties, 1, lambda fs: caseFilterMap[fs[0]] if fs[0] in caseFilterMap else None)
		caseRanges = Ranges.union(caseRanges, FilterProperties(unicodeData, 2, lambda fs: 3 if fs[1] == 'Lt' else None))
		file.beginBlock('Automatically generated from: Unicode derived property Lowercase, Uppercase or General_Category Lt', 'Case')
		file.writeEnum('CaseType', ['none', 'lowerCase', 'upperCase', 'titleCase'])
		WriteLookupFunction(file, 'GetCase', caseRanges, LookupType.Enum('CaseType', ['none', 'lowerCase', 'upperCase', 'titleCase'], 'none'))

		# write the category-enum to the file (https://www.unicode.org/reports/tr44/#GC_Values_Table)
		categoryEnumMap, categoryEnumList = {
			'Lu': 0, 'Ll': 1, 'Lt': 2, 'Lm': 3, 'Lo': 4, 'Mn': 5, 'Mc': 6, 'Me': 7, 'Nd': 8, 'Nl': 9, 'No': 10,
			'Pc': 11, 'Pd': 12, 'Ps': 13, 'Pe': 14, 'Pi': 15, 'Pf': 16, 'Po': 17, 'Sm': 18, 'Sc': 19, 'Sk': 20, 'So': 21,
			'Zs': 22, 'Zl': 23, 'Zp': 24, 'Cc': 25, 'Cf': 26, 'Cs': 27, 'Co': 28, 'Cn': 29
		}, ['lu', 'll', 'lt', 'lm', 'lo', 'mn', 'mc', 'me', 'nd', 'nl', 'no', 'pc', 'pd', 'ps', 'pe', 'pi', 'pf', 'po', 'sm', 'sc', 'sk', 'so', 'zs', 'zl', 'zp', 'cc', 'cf', 'cs', 'co', 'cn' ]
		categoryRanges = FilterProperties(unicodeData, 2, lambda fs: categoryEnumMap[fs[1]] if fs[1] in categoryEnumMap else None)
		file.beginBlock('Automatically generated from: Unicode General_Category', 'Category')
		file.writeEnum('CategoryType', categoryEnumList)
		WriteLookupFunction(file, 'GetCategory', categoryRanges, LookupType.Enum('CategoryType', categoryEnumList, 'cn'))

def MakeCodepointMaps():
	# parse the relevant files
	unicodeData = RawParseFile('generated/ucd/UnicodeData.txt', True)
	
	# create the uppercase diff-map
	# upperRanges = ExtractProperties(unicodeData, 11, { None: })

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
# MakeGraphemeTypeMapping()
