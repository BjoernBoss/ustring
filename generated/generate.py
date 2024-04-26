import urllib.request
import os
import sys
import datetime
import re

# ranges are lists of range-objects, which must be sorted and must not overlap/neighbor each other if same type
#	=> use Ranges.fromRawList to sort and merge an arbitrary list of Range objects
# ranges map [first-last] to a non-empty list of integers
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
	def distant(self, other: 'Range') -> bool:
		return (self.last + 1 < other.first or self.first - 1 > other.last)

class LookupType:
	def __init__(self) -> None:
		self._kind = ''
		self._typeName = ''
		self._values = []
		self._default = 0
		self._min = 0
		self._max = 0
	@staticmethod
	def boolType() -> 'LookupType':
		out = LookupType()
		out._kind = 'bool'
		out._typeName = 'bool'
		out._default = 0
		out._min = 0
		out._max = 1
		return out
	@staticmethod
	def selfIntType(defValue: int, intType: str, minValue: int, maxValue: int) -> 'LookupType':
		out = LookupType()
		out._kind = 'int'
		out._typeName = intType
		out._default = defValue
		out._min = minValue
		out._max = maxValue
		return out
	@staticmethod
	def intType(defValue: int, intType: str) -> 'LookupType':
		if intType == 'uint8_t':
			return LookupType.selfIntType(defValue, intType, 0, 2**8 - 1)
		elif intType == 'int8_t':
			return LookupType.selfIntType(defValue, intType, -2**(8 - 1), 2**(8 - 1) - 1)
		elif intType == 'uint16_t':
			return LookupType.selfIntType(defValue, intType, 0, 2**16 - 1)
		elif intType == 'int16_t':
			return LookupType.selfIntType(defValue, intType, -2**(16 - 1), 2**(16 - 1) - 1)
		elif intType == 'uint32_t':
			return LookupType.selfIntType(defValue, intType, 0, 2**32 - 1)
		elif intType == 'int32_t':
			return LookupType.selfIntType(defValue, intType, -2**(32 - 1), 2**(32 - 1) - 1)
		raise RuntimeError(f'Unknown integer type [{intType}] encountered')
	@staticmethod
	def rangeType(defValue: int, lower: int, upper: int) -> 'LookupType':
		# lookup the smallest suitable type to be used
		for i in [8, 16, 32]:
			if lower >= 0 and upper <= 2**i - 1:
				return LookupType.intType(defValue, f'uint{i}_t')
			elif lower >= -2**(i - 1) and upper <= 2**(i - 1) - 1:
				return LookupType.intType(defValue, f'int{i}_t')
		raise RuntimeError('No datatype suitable for buffer found')
	@staticmethod
	def listType(defValue: int, intList: list[int]) -> 'LookupType':
		if len(intList) == 0:
			return LookupType.rangeType(defValue, 0, 0)
		return LookupType.rangeType(defValue, min(intList), max(intList))
	@staticmethod
	def enumType(name: str, defValue: str, values: list[str]|dict) -> 'LookupType':
		if type(values) == dict:
			values, tMap = [None] * len(values), values
			for k in tMap:
				values[tMap[k]] = k
			if any(v is None for v in values):
				raise RuntimeError('Non-continuous enum-dictionary encountered')
		out = LookupType()
		out._kind = 'enum'
		out._typeName = f'{name}'
		out._values = values
		out._default = values.index(defValue)
		if len(out._values) == 0:
			raise RuntimeError(f'Enum [{name}] must not be empty')
		out._min = 0
		out._max = len(values) - 1
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
	def dynLookup(self, varName: str) -> str:
		if self._kind == 'bool':
			return f'({varName} != 0)'
		if self._kind == 'int':
			return f'{self._typeName}({varName})'
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
	def bufferSize(self) -> int:
		typeName = self.bufferType()
		for i in [8, 16, 32]:
			if typeName == f'int{i}_t' or typeName == f'uint{i}_t':
				return i // 8
		raise RuntimeError(f'Unsupported buffer-type [{typeName}] encountered')
	def bufferTypeBounds(self) -> tuple[int, int]:
		typeName = self.bufferType()
		for i in [8, 16, 32]:
			if typeName == f'int{i}_t':
				return [-2**(i - 1), 2**(i - 1) - 1]
			elif typeName == f'uint{i}_t':
				return [0, 2**i - 1]
		raise RuntimeError(f'Unsupported buffer-type [{typeName}] encountered')
	def validValue(self, value: int) -> bool:
		if self._kind not in ['bool', 'enum', 'int']:
			raise RuntimeError(f'Unknown kind [{self._kind}] encountered')
		return (value >= self._min and value <= self._max)

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
	def _conflictAppOrMerge(out: list[Range], other: Range, conflictHandler) -> None:
		# check if a simple append can be performed
		if len(out) == 0 or out[-1].distant(other):
			out.append(other)
			return

		# check if the ranges are neighbors and can be merged
		if out[-1].neighbors(other):
			if out[-1].values == other.values:
				out[-1] = out[-1].merge(other)
			else:
				out.append(other)
			return

		# check if the overlap is trivial, in which case a normal merge will be performed
		if out[-1].values == other.values:
			out[-1] = out[-1].merge(other)
			return

		# pop all conflicting ranges
		confCount = 1
		while confCount < len(out) and out[-confCount - 1].last >= other.first:
			confCount += 1
		conflict = out[-confCount:]
		for _ in range(confCount):
			out.pop()

		# remove any initial non-conflicting region
		if conflict[0].first < other.first:
			leading, conflict[0] = conflict[0].split(other.first - conflict[0].first)
			out.append(leading)

		# iterate over the conflicting range and process it
		otherRemaining: Range|None = other
		while len(conflict) > 0 and otherRemaining is not None:
			# extract the left and right conflicting range
			if conflict[0].span() == otherRemaining.span():
				left, right, conflict, otherRemaining = conflict[0], otherRemaining, [], None
			elif conflict[0].span() < otherRemaining.span():
				left, conflict = conflict[0], conflict[1:]
				right, otherRemaining = other.split(left.span())
			else:
				left, conflict[0] = conflict[0].split(otherRemaining.span())
				right, otherRemaining = otherRemaining, None

			# check if the conflict is trivial or left the handler process it
			if left.values == right.values:
				out.append(left.merge(right))
			else:
				out.append(Range(left.first, left.last, conflictHandler(left.values, right.values)))

		# append the remainder to the output
		for c in conflict:
			out.append(c)
		if otherRemaining is not None:
			out.append(otherRemaining)
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
	def _conflictUnionOperation(a: tuple[int]|None, b: tuple[int]|None, conflictHandler) -> tuple[int]|None:
		if a is None:
			return b
		if b is None:
			return a
		return conflictHandler(a, b)

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
	def fromConflictingRawList(ranges: list[Range], conflictHandler) -> list[Range]:
		# sort the ranges
		ranges = sorted(ranges, key=lambda r : r.first)

		# merge any neighboring/overlapping ranges of the same type and let the handler process conflicting overlaps
		out: list[Range] = []
		for r in ranges:
			Ranges._conflictAppOrMerge(out, r, conflictHandler)
		return out
	@staticmethod
	def wellFormed(ranges: list[Range], lookupType: LookupType) -> None:
		# check if the default-type can be held by the type
		if not lookupType.validValue(lookupType.defValue()):
			raise RuntimeError('Invalid default-value for type encountered')
		for i in range(len(ranges)):
			if i > 0 and ranges[i - 1].first > ranges[i].first:
				raise RuntimeError('Order of ranges violation encountered')
			if i > 0 and ranges[i - 1].last + 1 > ranges[i].first:
				raise RuntimeError('Overlapping ranges encountered')
			for v in ranges[i].values:
				if not lookupType.validValue(v):
					raise RuntimeError('Invalid value for type encountered')
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
	def merge(a: list[Range], b: list[Range], conflictHandler) -> list[Range]:
		return Ranges._setIteration(a, b, False, lambda a, b: Ranges._conflictUnionOperation(a, b, conflictHandler))
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

class CodeGen:
	ListConsideredSparse = 0.3
	IndirectBufferShiftTests = [0, 1, 2, 3, 4, 5, 6, 7]
	IndirectBufferNestingLevel = 3
	IndirectBufferMinSize = 128
	LookupAsCodeIfLEQRanges = 96
	CodeGenDensityThreshold = 1 / 2.5
	DensityMinClusterSize = 8
	SeparateAsciiRanges = 48
	MaxBinarySearchValueSize = 0x10000

	def __init__(self, file: 'GeneratedFile', blockName: str, desc: str) -> None:
		# global data
		self._file = file
		self._file.beginBlock(desc)
		self._block = blockName
		self._frames = []
		self._varIndex = 0
		self._buffers: list[tuple[str, list[int], LookupType]] = []

		# affected by frames
		self._code = ''
		self._hint = ''
		self._inVar = ''
		self._outTo = ''
		self._type = None

	def _pushContext(self, tp: LookupType, inVarName: str, outWriteTo: str, hint: str) -> None:
		self._frames.append((self._type, self._code, self._hint, self._inVar, self._outTo))
		self._type = tp
		self._code = ''
		self._hint += hint
		self._inVar = inVarName
		self._outTo = outWriteTo
	def _popContext(self) -> str:
		code = self._code
		self._type, self._code, self._hint, self._inVar, self._outTo = self._frames.pop()
		return code
	def _writeBuffers(self) -> None:
		for (name, data, tp) in self._buffers:
			# slightly align the count to get a closer resembles of rectangles
			valsPerLine = 24
			estimatedLines = (len(data) + valsPerLine - 1) // valsPerLine
			valsPerLine = (len(data) + estimatedLines - 1) // estimatedLines

			# select the formatting precision based on the type
			btUnsigned = (tp.bufferType() == 'uint8_t')
			btSigned = (tp.bufferType() == 'int8_t')

			# write the header, data, and trailer out
			self._file.write(f'static constexpr {tp.bufferType()} {name}[{len(data)}] = {{\n\t')
			for i in range(len(data)):
				if i > 0:
					self._file.write(f',{"\n\t" if (i % valsPerLine) == 0 else ""}')
				if btUnsigned:
					self._file.write(f' {data[i]:#04x}')
				elif btSigned:
					self._file.write(f' {data[i]: #05x}')
				else:
					self._file.write(f' {data[i]: #07x}')
			self._file.writeln('\n};')

	def _value(self, val: int) -> str:
		return f'{val:#06x}'
	def _addBuffer(self, data: list[int], tp: LookupType, hint: str) -> str:
		# check if the type is suited for the data
		lowerBound, upperBound = tp.bufferTypeBounds()
		if any(d < lowerBound or d > upperBound for d in data):
			raise RuntimeError(f'Type [{tp.typeName()}] is not suited for data')

		# setup the buffer name and define it
		name = f'{self._block}Buf{len(self._buffers)}{self._hint}{hint}'
		self._buffers.append((name, data, tp))
		return f'gen::{name}'
	def _addVar(self, hint: str) -> str:
		name = f'var{self._varIndex}{self._hint}{hint}'
		self._varIndex += 1
		return name
	def _condition(self, ranges: list[Range], checkLower: bool, checkUpper: bool, varName: str) -> str:
		out = '('

		# iterate over the ranges to be tested and add them to the condition (check greater/less equal before equality, to ensure open sides are handled properly)
		for i in range(len(ranges)):
			if i > 0:
				out += ' || '

			first, last = self._value(ranges[i].first), self._value(ranges[i].last)
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

	def _density(self, ranges: list[Range], densityThreshold: float, minClusterSize: int) -> list[tuple[int, int]]:
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
			if bestDensity < densityThreshold:
				break
			l, r = clusters[best], clusters[best + 1]
			clusters[best] = (l[0], r[1], l[2] + r[2], l[3] + r[3])
			clusters = clusters[:best + 1] + clusters[best + 2:]

		# remove all clusters, which either did not reach the density or chars threshold
		index = 0
		while index < len(clusters):
			if clusters[index][3] >= minClusterSize and (clusters[index][2] / clusters[index][3]) >= densityThreshold:
				index += 1
			else:
				clusters = clusters[:index] + clusters[index + 1:]

		# transform the clusters to the required output format (will be sorted as the algorithm does not reorder them)
		return [(clusters[i][0], clusters[i][1]) for i in range(len(clusters))]
	def _analyze(self, ranges: list[Range]) -> dict[list[int], list[int]]:
		out: dict[list[int], list[int]] = {}
		for i in range(len(ranges)):
			if ranges[i].values not in out:
				out[ranges[i].values] = []
			out[ranges[i].values].append(i)
		return out
	def _rangeFromList(self, data: list[int]) -> list[Range]:
		return Ranges.fromRawList([Range(i, i, data[i]) for i in range(len(data))])

	def _binarySearchLookup(self, data: list[int], tp: LookupType, inVarName: str, outWriteTo: str, hint: str, bufMaxNesting: int, bufShiftTests: list[int], bufMinIndSize: int) -> str:
		self._pushContext(tp, inVarName, outWriteTo, hint)
		self._indirectBuffer(self._rangeFromList(data), bufMaxNesting, bufShiftTests, bufMinIndSize)
		return self._popContext()
	def _binarySearch(self, ranges: list[Range], maxBinSearchSize: int, bufMaxNesting: int, bufShiftTests: list[int], bufMinIndSize: int) -> None:
		defValue = None

		# check if the last value will be the default value (due to the size-constraints, any too large last-values can produce exeedingly many values)
		if (ranges[-1].span() // maxBinSearchSize) >= len(ranges):
			defValue = ranges[-1].values
			ranges = ranges[:-1] + [Range(ranges[-1].first, ranges[-1].first + maxBinSearchSize - 1, defValue)]

		# update the ranges to adhere to the size-constraints
		ranges: list[Range] = Ranges.limitSize(ranges, maxBinSearchSize)

		# lookup the base value to be used as default and filter it from the ranges (dont filter the first-most value, as it is necessary to be an element in the binary search-buffer)
		counts = self._analyze(ranges)
		if defValue is None:
			for value in counts:
				if defValue is None or len(counts[defValue]) < len(counts[value]):
					defValue = value
		ranges = ranges[0:1] + Ranges.filter(ranges[1:], defValue)

		# setup the initial state of the variables required
		leftVarName, rightVarName = self._addVar('Left'), self._addVar('Right')
		centerVarName, sizeVarName = self._addVar('Center'), self._addVar('Size')

		# check if there are more than two values, in which case a lookup for the values is required and otherwise return any value from the range
		if len(counts) > 2:
			getValueCode = self._binarySearchLookup([r.values[0] for r in ranges], self._type, leftVarName, self._outTo, 'Value', bufMaxNesting, bufShiftTests, bufMinIndSize)
			if StrHelp.multiLines(getValueCode):
				getValueCode = f' {{\n{StrHelp.indent(getValueCode)}}}\n'
			else:
				getValueCode = f'\n\t{getValueCode}'
		else:
			getValueCode = f'\n\t{self._outTo} {self._type.staticLookup(ranges[0].values[0])};\n'

		# create the lookup code for the ranges sizes (store the size smaller by 1 as a size cannot be 0)
		sizeBufferList = [r.span() - 1 for r in ranges]
		sizeBufferType = LookupType.listType(-1, sizeBufferList)
		getSizeCode = self._binarySearchLookup(sizeBufferList, sizeBufferType, leftVarName, f'{sizeVarName} =', 'Size', bufMaxNesting, bufShiftTests, bufMinIndSize)

		# write the buffer for the first-values out (must be a buffer, as it will be passed
		# to the binary-search and will hopefully be complex enough to require a binary-search, and ensure all values are smaller than maxBinSearchSize)
		startBufferList = [(r.first - ranges[0].first) % maxBinSearchSize for r in ranges]
		startBufferType = LookupType.listType(-1, startBufferList)
		startBufferName = self._addBuffer(startBufferList, startBufferType, 'Start')

		# setup the start-buffer offset list and allocate it
		tempStartBufferOffsets = self._rangeFromList([(r.first - ranges[0].first) // maxBinSearchSize for r in ranges])
		startBufferOffsets = []
		for i in range(len(tempStartBufferOffsets)):
			# add all starts (both the actual, and fill all intermediate holes with the same start)
			diff: int = (1 if i == 0 else tempStartBufferOffsets[i].values[0] - tempStartBufferOffsets[i - 1].values[0])
			startBufferOffsets += [tempStartBufferOffsets[i].first] * diff
		startBufferOffsets.append(tempStartBufferOffsets[-1].last + 1)
		startBufferOffsetName = self._addBuffer(startBufferOffsets, LookupType.listType(0, startBufferOffsets), 'Offset')

		# add the initial code to offset the input-var correctly
		if ranges[0].first > 0:
			self._code += f'{self._inVar} -= {self._value(ranges[0].first)};\n\n'

		# add the code to compute the left and right edge of the binary search
		self._code += f'{sizeBufferType.typeName()} {sizeVarName} = 0;\n'
		self._code += f'size_t {leftVarName} = std::min<size_t>({self._inVar} / {self._value(maxBinSearchSize)}, {len(startBufferOffsets) - 2});\n'
		self._code += f'size_t {rightVarName} = {startBufferOffsetName}[{leftVarName} + 1] - 1;\n'
		self._code += f'{leftVarName} = {startBufferOffsetName}[{leftVarName}];\n'
		self._code += '\n'

		# add the code to perform the binary search
		self._code += f'while ({leftVarName} < {rightVarName}) {{\n'
		self._code += f'\tsize_t {centerVarName} = ({leftVarName} + {rightVarName} + 1) / 2;\n'
		self._code += f'\tif ({self._inVar} < {startBufferName}[{centerVarName}])\n'
		self._code += f'\t\t{rightVarName} = {centerVarName} - 1;\n'
		self._code += f'\telse\n'
		self._code += f'\t\t{leftVarName} = {centerVarName};\n'
		self._code += f'}}\n'
		self._code += '\n'

		# add the code to fetch the size
		self._code += getSizeCode
		self._code += '\n'

		# add the negative size condition (check input-var greater than size, as size is reduced by one)
		self._code += f'if ({self._inVar} - {startBufferName}[{leftVarName}] > {sizeVarName})\n'
		self._code += f'\t{self._outTo} {self._type.staticLookup(defValue[0])};\n'

		# add the code to perform the value-lookup
		self._code += 'else' + getValueCode

	def _ifElseClusters(self, ranges: list[Range], densityThreshold: float, minClusterSize: int) -> None:
		# perform density-clustering to detect what areas might benefit from buffers
		clusters, clusterAccess = self._density(ranges, densityThreshold, minClusterSize), []

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
				clusterAccess.append(self._inVar)
				if offset != 0:
					clusterAccess[-1] += (' + ' if offset > 0 else ' - ') + f'{self._value(abs(offset))}'

			# write the buffer out and patch the access-strings
			bufferName = self._addBuffer(clusterData, self._type, 'Cluster')
			for i in range(len(clusterAccess)):
				clusterAccess[i] = self._type.dynLookup(f'{bufferName}[{clusterAccess[i]}]')
				clusterAccess[i] = f'{self._outTo} {clusterAccess[i]};\n'

		# analyze the remaining ranges
		state, active = self._analyze(ranges), { i for i in range(len(ranges)) }

		# iterate until all ranges have been processed/inserted
		statementCount = -1
		while True:
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
				selectedValue = f'{self._outTo} {self._type.staticLookup(bestValues[0])};\n'

			# check if these were the last ranges
			if len(active) == 0:
				if statementCount > 0:
					self._code += 'else\n\t'
				self._code += selectedValue
				return

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
				self._code += ("else if " if statementCount > 0 else "if ") + self._condition(sortedActual[index:index + count], thisLower, thisUpper, self._inVar)
				if StrHelp.multiLines(selectedValue):
					self._code += ' {\n' + StrHelp.indent(selectedValue) + '}\n'
				else:
					self._code += '\n' + StrHelp.indent(selectedValue)
				index += count

	def _tryIndirectBuffer(self, ranges: list[Range], shift: int) -> tuple[list[int], list[int]]:
		indexBuffer: list[int] = []
		dataBuffer: list[int] = []
		indexMap: dict[tuple, int] = {}

		# check if this is a simple access (i.e. no indirection), in which case only a data-buffer is returned
		if shift == 0:
			dataBuffer = [r.values[0] for r in ranges for _ in range(r.first, r.last + 1)]
			return (dataBuffer, indexBuffer)
		count, rangeIndex = (1 << shift), 0

		# iterate over the data and split them up and populate the indices-map
		for i in range(ranges[0].first, ranges[-1].last + 1, count):
			listPackage = []

			# fetch the next data from the range from [i : i + count]
			for j in range(count):
				if rangeIndex >= len(ranges):
					listPackage.append(0)
				else:
					listPackage.append(ranges[rangeIndex].values[0])
					if i + j == ranges[rangeIndex].last:
						rangeIndex += 1
			tuplePackage = tuple(listPackage)

			# lookup the new index for the package and assign it
			if tuplePackage not in indexMap:
				indexMap[tuplePackage] = len(indexMap)
				dataBuffer += listPackage
			indexBuffer.append(indexMap[tuplePackage])
		return (dataBuffer, indexBuffer)
	def _setupIndirectBuffers(self, ranges: list[Range], tp: LookupType, nestingLevel: int, shiftList: list[int], minIndirectSize: int) -> list[tuple[int, list[int], list[int]]]:
		# (shift-count, dataBuffer, indexBuffer)
		out: list[tuple[int, list[int], list[int]]] = []
		outMemoryCost: int = 0

		# try all variations and find the least memory-intensive break
		for i in shiftList:
			# try the next direct combination
			dataBuffer, indexBuffer = self._tryIndirectBuffer(ranges, i)
			indexType: LookupType = LookupType.rangeType(0, 0, (len(dataBuffer) // (1 << i)) - 1)

			# setup the temporary result
			tempOut = [(i, dataBuffer, indexBuffer)]
			tempMemoryCost: int = len(dataBuffer) * tp.bufferSize() + len(indexBuffer) * indexType.bufferSize()

			# check if the index-buffer can be split further (ignore direct buffers as they are equivalent to the primitive selection)
			if nestingLevel > 0 and i > 0 and len(indexBuffer) > minIndirectSize:
				nested = self._setupIndirectBuffers(self._rangeFromList(indexBuffer), indexType, nestingLevel - 1, shiftList, minIndirectSize)

				# compute the cost of the nested result
				nestCost: int = 0
				for (_, datBuffer, idxBuffer) in nested:
					nestCost += LookupType.listType(0, datBuffer).bufferSize() * len(datBuffer)
					nestCost += LookupType.listType(0, idxBuffer).bufferSize() * len(idxBuffer)

				# check if the nested result is cheaper than the current index buffer
				if nested[0][0] > 0 and nestCost < len(indexBuffer) * indexType.bufferSize():
					tempOut = [(i, dataBuffer, [])] + nested
					tempMemoryCost = len(dataBuffer) * tp.bufferSize() + nestCost

			# check if the new found solution results in a smaller memory footprint
			if len(out) == 0 or tempMemoryCost < outMemoryCost:
				out = tempOut
				outMemoryCost = tempMemoryCost
		return out
	def _indirectBuffer(self, ranges: list[Range], nestingLevel: int, shiftList: list[int], minIndirectSize: int) -> None:
		lowerClipped, upperClipped = False, False

		# check if the range needs to be clipped to prevent too large buffers
		if len(ranges) > 2:
			leftEdgeSpan, rightEdgeSpan, centerSpan = ranges[0].span(), ranges[-1].span(), (ranges[-1].first - 1) - (ranges[0].last + 1)
			lowerClipped = (leftEdgeSpan >= centerSpan)
			upperClipped = (rightEdgeSpan >= centerSpan)

		# setup the clipped ranges and extract the type for the buffer to be used (dont use the type itself to ensure minimal size)
		clippedRanges = ranges[(1 if lowerClipped else 0): (-1 if upperClipped else len(ranges))]
		bufType: LookupType = LookupType.rangeType(0, min(r.values[0] for r in clippedRanges), max(r.values[0] for r in clippedRanges))

		# check if the bounds need to be checked
		if lowerClipped:
			self._code += f'if ({self._inVar} <= {self._value(ranges[0].last)})\n'
			self._code += f'\t{self._outTo} {self._type.staticLookup(ranges[0].values[0])};\n'
		if upperClipped:
			self._code += ('else ' if lowerClipped else '') + f'if ({self._inVar} >= {self._value(ranges[-1].first)})\n'
			self._code += f'\t{self._outTo} {self._type.staticLookup(ranges[-1].values[0])};\n'

		# add the else-block opening
		indent = ''
		if lowerClipped or upperClipped:
			self._code += 'else {\n'
			indent = '\t'

		# select the list of shifts, data-buffers, index-buffers
		indirections = self._setupIndirectBuffers(clippedRanges, bufType, nestingLevel, shiftList, minIndirectSize)

		# setup the small and large buffer as well as the mapping to the corresponding slots
		indexMap: dict[int|None, tuple[int, bool]] = {}
		smBufferData, lgBufferData = [], []
		for i in range(len(indirections)):
			_, datBuffer, idxBuffer = indirections[i]

			# write the data to the buffer
			if LookupType.listType(0, datBuffer).bufferSize() == 1:
				indexMap[i] = (len(smBufferData), True)
				smBufferData += datBuffer
			else:
				indexMap[i] = (len(lgBufferData), False)
				lgBufferData += datBuffer

			# write the index to the buffer (there can only exist one index-buffer)
			if len(idxBuffer) > 0:
				if LookupType.listType(0, idxBuffer).bufferSize() == 1:
					indexMap[None] = (len(smBufferData), True)
					smBufferData += idxBuffer
				else:
					indexMap[None] = (len(lgBufferData), False)
					lgBufferData += idxBuffer

		# check if the buffer(s) need to be allocated
		smBufferName, lgBufferName = '', ''
		if len(smBufferData) > 0:
			smBufferName = self._addBuffer(smBufferData, LookupType.listType(0, smBufferData), ('IndData' if len(lgBufferData) == 0 else 'IndSmall'))
		if len(lgBufferData) > 0:
			lgBufferName = self._addBuffer(lgBufferData, LookupType.listType(0, lgBufferData), ('IndData' if len(smBufferData) == 0 else 'IndLarge'))

		# allocate and initialize the in-var
		inVarName = self._addVar('In')
		self._code += f'{indent}size_t {inVarName} = size_t({self._inVar})'
		if clippedRanges[0].first > 0:
			self._code += f' - {self._value(clippedRanges[0].first)}'
		self._code += ';\n'

		# compute the initial total shift to be applied to the input variable and progressively reduced
		shiftSum: int = sum(ind[0] for ind in indirections)

		# check if its only a single direct lookup
		if shiftSum == 0:
			indexLookupCode = f'{smBufferName if len(smBufferName) > 0 else lgBufferName}[{inVarName}]'
			self._code += f'{indent}{self._outTo} {self._type.dynLookup(indexLookupCode)};\n'
			if lowerClipped or upperClipped:
				self._code += '}\n'
			return

		# add the initial (and only) index lookup, as all other indices will be computed and looked up from the data-buffer
		indexVarName = self._addVar('Index')
		indexLookupCode = f'{inVarName} >> {shiftSum}'
		if indexMap[None][0] > 0:
			indexLookupCode = f'{self._value(indexMap[None][0])} + ({indexLookupCode})'
		self._code += f'{indent}size_t {indexVarName} = {smBufferName if indexMap[None][1] else lgBufferName}[{indexLookupCode}];\n'

		# iterate over the indirections and apply them to the index-variable
		for i in range(len(indirections) - 1, -1, -1):
			shift = indirections[i][0]
			dataOffset, dataSmall = indexMap[i]

			# add the operation to compute the index into the data-buffer for the current indirection
			shiftSum -= shift
			indexLookupCode = (f'({inVarName} >> {shiftSum})' if shiftSum > 0 else inVarName)
			indexLookupCode = f'({indexVarName} << {shift}) + ({indexLookupCode} & {(1 << shift) - 1:#04x})'

			# add the memory lookup (i.e. the 'data')
			if dataOffset > 0:
				indexLookupCode = f'{smBufferName if dataSmall else lgBufferName}[{indexLookupCode} + {dataOffset:#04x}]'
			else:
				indexLookupCode = f'{smBufferName if dataSmall else lgBufferName}[{indexLookupCode}]'

			# add the line to the produced code, as well as the final dynamic lookup
			self._code += f'{indent}{indexVarName} = {indexLookupCode};\n'
			if i == 0:
				self._code += f'{indent}{self._outTo} {self._type.dynLookup(indexVarName)};\n'

		# add the closing indentation
		if lowerClipped or upperClipped:
			self._code += '}\n'

	def _lookupRanges(self, ranges: list[Range], tp: LookupType, addIndentation: bool, inVarName: str, outWriteTo: str, hint: str) -> str:
		# check if the range is empty or trivial and nothing needs to be done
		if len(ranges) == 0:
			return f'{"\n\t" if addIndentation else ""}{outWriteTo} {tp.staticLookup(tp.defValue())};\n'
		if len(ranges) == 1:
			return f'{"\n\t" if addIndentation else ""}{outWriteTo} {tp.staticLookup(ranges[0].values[0])};\n'

		# setup the new context for the execution
		self._pushContext(tp, inVarName, outWriteTo, hint)

		# check if a code-lookup makes sense (i.e. few ranges)
		if len(ranges) <= CodeGen.LookupAsCodeIfLEQRanges:
			self._ifElseClusters(ranges, CodeGen.CodeGenDensityThreshold, CodeGen.DensityMinClusterSize)

		# either perform a binary search or use indirect buffers (currently: indirect-buffers have a lower memory-footprint and fewer memory accesses)
		else:
			self._indirectBuffer(ranges, CodeGen.IndirectBufferNestingLevel, CodeGen.IndirectBufferShiftTests, CodeGen.IndirectBufferMinSize)
			# self._binarySearch(ranges, CodeGen.MaxBinarySearchValueSize, CodeGen.IndirectBufferNestingLevel, CodeGen.IndirectBufferShiftTests, CodeGen.IndirectBufferMinSize)

		# fetch the produced code
		lookupCode: str = self._popContext()

		# check if brackets need to be added
		if addIndentation:
			if StrHelp.multiLines(lookupCode):
				lookupCode = ' {\n' + StrHelp.indent(lookupCode) + '}\n'
			else:
				lookupCode = '\n\t' + lookupCode
		return lookupCode
	def _fullRangeLookup(self, ranges: list[Range], tp: LookupType, inVarName: str, outVarName: str|None) -> str:
		# initialize the out variable-name
		if outVarName is None:
			outVarName = 'return'
		else:
			outVarName += ' ='
		lookupCode = ''

		# check if the entire ranges should be handled as a single lookup
		if len(ranges) < CodeGen.SeparateAsciiRanges or ranges[-1].first <= 0x80:
			return self._lookupRanges(ranges, tp, False, inVarName, outVarName, '')

		# add the ascii lookup
		lookupCode += f'if ({inVarName} < 0x80)'
		asciiRanges, upperRanges = Ranges.split(ranges, 0x80)
		lookupCode += self._lookupRanges(asciiRanges, tp, True, inVarName, outVarName, 'Ascii')

		# add the remaining lookup code
		lookupCode += 'else'
		lookupCode += self._lookupRanges(upperRanges, tp, True, inVarName, outVarName, '')
		return lookupCode

	def addConstInt(self, type: LookupType, name: str, value: int) -> None:
		value = hex(value)[2:]
		value = '0' * (type.bufferSize() * 2 - len(value)) + value
		self._file.writeln(f'static constexpr {type.typeName()} {name} = 0x{value};')
	def addEnum(self, enum: LookupType) -> None:
		# write the enum out
		self._file.write(f'enum class {enum.typeName(True)} : {enum.bufferType()} {{\n\t')
		self._file.writeln(",\n\t".join(enum.enumValues()))
		self._file.writeln('};')
	def intFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range]) -> None:
		print(f'Creating integer-lookup {fnName}...')
		Ranges.wellFormed(ranges, lookupType)

		# fill all holes compared to the entire valid range with the default value
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# generate the lookup-code and write the final buffers out
		lookupCode: str = self._fullRangeLookup(ranges, lookupType, 'cp', None)
		self._writeBuffers()

		# generate the actual function and insert the final code
		self._file.writeln(f'inline constexpr {lookupType.typeName()} {fnName}(char32_t cp) {{')
		self._file.write(StrHelp.indent(lookupCode))
		self._file.writeln('}')
	def listFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range]) -> None:
		print(f'Creating list-lookup {fnName}...')
		Ranges.wellFormed(ranges, lookupType)

		# fill the entire valid range with the default value (must be done before setting up the
		#	data-buffer as the default-value will otherwise not be written to the data-buffer)
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# check if a sparse buffer should be created for the map (data-buffer contains either single value, or null
		#	followed by size followed by values; null-value also needs special encoding) or simply write size, and data out
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

		# define the actual data-buffer and allocate the index-variable to be used
		dataBufferName: str = self._addBuffer(dataBuffer, lookupType, 'Data')
		indexVarName: str = self._addVar('Index')

		# generate the lookup-code and write the final buffers out
		lookupCode: str = self._fullRangeLookup(ranges, LookupType.rangeType(0, 0, len(dataIndex) - 1), 'cp', indexVarName)
		self._writeBuffers()

		# generate the actual function and insert the generated code
		self._file.writeln(f'inline constexpr std::pair<size_t, const {lookupType.typeName()}*> {fnName}(char32_t cp) {{')
		self._file.writeln(f'\tsize_t {indexVarName} = 0;')
		self._file.write(StrHelp.indent(lookupCode))

		# add the logic to extract the size and data
		if sparseBuffer:
			self._file.writeln(f'\tif ({dataBufferName}[{indexVarName}] != 0)')
			self._file.writeln(f'\t\treturn {{ 1, {dataBufferName} + {indexVarName} }};')
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName} + 1]), {dataBufferName} + {indexVarName} + 2 }};')
		else:
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName}]), {dataBufferName} + {indexVarName} + 1 }};')
		self._file.writeln('}')

class GenerateConfig:
	def __init__(self, url: str, version: str, date: str, includes: list[str]) -> None:
		self.url = url
		self.version = version
		self.date = date
		self.includes = includes

class GeneratedFile:
	def __init__(self, path: str, config: GenerateConfig) -> None:
		self._path = path
		self._config = config
		self._file = None
		self._hadFirstBlock = False
		self._atStartOfLine = False
		self._indented = False
	def __enter__(self) -> 'GeneratedFile':
		self._file = open(self._path, mode='w', encoding='ascii')
		self._atStartOfLine = True

		# write the file header
		self.writeln('#pragma once')
		self.writeln('')
		for include in self._config.includes:
			self.writeln(f'#include <{include}>')
		self.writeln('')
		self._writeComment('This is an automatically generated file and should not be modified.\n'
				  + 'All data are based on the lastest information provided by the unicode character database.\n'
				  + f'Source URL: {self._config.url}\n'
				  + f'Generated on: {self._config.date}\n'
				  + f'Generated from version: {self._config.version}', False)
		self.writeln('namespace cp::detail::gen {')
		self._indented = True
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
	def next(self, blockName: str, desc: str) -> CodeGen:
		return CodeGen(self, blockName, desc)

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
	def filter(self, relevantFields: int, assignValue, failIfNotFound: bool = False, conflictHandler = None) -> list[Range]:
		ranges: list[Range] = []

		# iterate over the parsed lines and match them against the callback
		for (begin, last, _, fields) in self._parsed:
			# check if the line can be ignored
			if len(fields) < relevantFields:
				if failIfNotFound:
					raise RuntimeError(f'Invalid range [{begin:#06x} - {last:#06x}] encountered in filter')
				continue

			# fetch the assigned value and check if the entry is to be ignored
			value = assignValue(fields)
			if value is not None:
				ranges.append(Range(begin, last, value))

		# sanitize and cleanup the found ranges
		if conflictHandler is None:
			return Ranges.fromRawList(ranges)
		return Ranges.fromConflictingRawList(ranges, conflictHandler)
	def extractAll(self, fieldCount: int, relevantField: int, valueMap: dict[str, int]) -> tuple[list[Range], int]:
		default, ranges = None, []

		# iterate over the parsed lines and validate them and extract the range-objects
		for (begin, last, missing, fields) in self._parsed:
			# check if the line can be ignored
			if len(fields) < fieldCount:
				raise RuntimeError(f'Too few fields in line [{begin:06x} - {last:06x}]')
				continue
			field = fields[relevantField]

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

# download all relevant files from the latest release of the ucd and extract the version (unicode character database: https://www.unicode.org/Public/UCD/latest)
def DownloadUCDFiles(refreshFiles: bool, baseUrl: str) -> str:
	files = {
		'ReadMe.txt': 'ReadMe.txt',
		'UnicodeData.txt': 'ucd/UnicodeData.txt', 
		'PropList.txt': 'ucd/PropList.txt', 
		'DerivedCoreProperties.txt': 'ucd/DerivedCoreProperties.txt',
		'SpecialCasing.txt': 'ucd/SpecialCasing.txt',
		'WordBreakProperty.txt': 'ucd/auxiliary/WordBreakProperty.txt',
		'EmojiData.txt': 'ucd/emoji/emoji-data.txt'
	}
	dirPath = './ucd'

	# check if the directory needs to be created
	if not os.path.isdir(dirPath):
		os.mkdir(dirPath)

	# download all of the files (only if they should either be refreshed, or do not exist yet)
	for file in files:
		url, path = f'{baseUrl}/{files[file]}', f'{dirPath}/{file}'
		if not refreshFiles and os.path.isfile(path):
			print(f'skipping [{path}] as the file already exists (use --refresh to enforce a new download)')
			continue
		print(f'downloading [{url}] to [{path}]...')
		urllib.request.urlretrieve(url, path)

	# fetch the version from the read-me
	with open('ucd/ReadMe.txt', 'r') as f:
		fileContent = f.read()
	version = re.findall('Version ([0-9]+(\\.[0-9]+)*) of the Unicode Standard', fileContent)
	if len(version) != 1:
		raise RuntimeError('Unable to extract the version')
	return version[0][0]

# TestAscii, TestAlpha, GetRadix, GetDigit, TestWhiteSpace, TestControl, TestLetter, GetPrintable, GetCase, GetCategory
def MakeCodepointQuery(config: GenerateConfig):
	# parse the relevant files
	unicodeData = ParsedFile('ucd/UnicodeData.txt', True)
	derivedProperties = ParsedFile('ucd/DerivedCoreProperties.txt', False)
	propList = ParsedFile('ucd/PropList.txt', False)

	# write all lookup functions to the file
	with GeneratedFile('unicode-cp-query.h', config) as file:
		# write the unicode-test to the file
		unicodeRanges = Ranges.difference([Range(0, 0x10ffff, 1)], unicodeData.filter(2, lambda fs: None if fs[1] != 'Cs' else 1))
		_gen: CodeGen = file.next('Unicode', 'Automatically generated from: Unicode General_Category is not cs (i.e. surrogate pairs) smaller than/equal to 0x10ffff')
		_gen.intFunction('TestUnicode', LookupType.boolType(), unicodeRanges)

		# write the assigned-test to the file
		assignedRanges = unicodeData.filter(2, lambda fs: None if fs[1] in ['Cs', 'Co', 'Cn'] else 1)
		_gen: CodeGen = file.next('Assigned', 'Automatically generated from: Unicode General_Category is not Cn, Cs, Co (i.e. not assigned, surrogate pairs, private use)')
		_gen.intFunction('TestAssigned', LookupType.boolType(), assignedRanges)

		# write the ascii-test to the file
		asciiRanges = [Range(0, 0x7f, 1)]
		_gen: CodeGen = file.next('Ascii', 'Automatically generated from: [cp <= 0x7f]')
		_gen.intFunction('TestAscii', LookupType.boolType(), asciiRanges)

		# write the alpha-test to the file
		alphaRanges = Ranges.fromRawList([Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)])
		_gen: CodeGen = file.next('Alpha', 'Automatically generated from: [a-zA-Z]')
		_gen.intFunction('TestAlpha', LookupType.boolType(), alphaRanges)

		# write the radix-getter to the file
		radixRanges = Ranges.fromRawList([Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)])
		_gen: CodeGen = file.next('Radix', 'Automatically generated from: [0-9a-zA-Z] mapped to [0-35] and rest to 0xff')
		_gen.intFunction('GetRadix', LookupType.intType(0xff, 'uint8_t'), radixRanges)

		# write the digit-getter to the file (https://www.unicode.org/reports/tr44/#Numeric_Type)
		digitRanges = unicodeData.filter(8, lambda fs: int(fs[5]) if fs[5] != '' and fs[5] in '0123456789' else None)
		digitRanges = Ranges.merge(digitRanges, unicodeData.filter(8, lambda fs: 0xf0 if fs[6] != '' else None), lambda a, b: a)
		digitRanges = Ranges.merge(digitRanges, unicodeData.filter(8, lambda fs: 0xf1 if fs[7] != '' else None), lambda a, b: a)
		_gen: CodeGen = file.next('Digit', 'Automatically generated from: Unicode: Numeric_Type=Decimal: [0-9]; Numeric_Type=Digit: [0xf0]; Numeric_Type=Numeric: [0xf1]; rest [0xff]')
		_gen.intFunction('GetDigit', LookupType.intType(0xff, 'uint8_t'), digitRanges)

		# write the whitespace-test to the file (https://www.unicode.org/reports/tr44/#White_Space)
		whiteSpaceRanges = propList.filter(1, lambda fs: None if fs[0] != 'White_Space' else 1)
		_gen: CodeGen = file.next('WhiteSpace', 'Automatically generated from: Unicode White_Space property')
		_gen.intFunction('TestWhiteSpace', LookupType.boolType(), whiteSpaceRanges)

		# write the control-test to the file (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
		controlRanges = unicodeData.filter(2, lambda fs: None if fs[1] != 'Cc' else 1)
		_gen: CodeGen = file.next('Control', 'Automatically generated from: Unicode General_Category is cc (i.e. C0, C1)')
		_gen.intFunction('TestControl', LookupType.boolType(), controlRanges)

		# write the letter-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic)
		letterRanges = derivedProperties.filter(1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		_gen: CodeGen = file.next('Letter', 'Automatically generated from: Unicode derived property Alphabetic')
		_gen.intFunction('TestLetter', LookupType.boolType(), letterRanges)

		# write the alpha-num-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic + https://www.unicode.org/reports/tr44/#Numeric_Type)
		alnumRanges = derivedProperties.filter(1, lambda fs: None if fs[0] != 'Alphabetic' else 1)
		alnumRanges = Ranges.union(alnumRanges, unicodeData.filter(8, lambda fs: 1 if fs[7] != '' else None))
		_gen: CodeGen = file.next('AlNum', 'Automatically generated from: Unicode derived property Alphabetic or Numeric_Type=Decimal,Digit,Numeric')
		_gen.intFunction('TestAlNum', LookupType.boolType(), alnumRanges)

		# write the printable-enum to the file (https://en.wikipedia.org/wiki/Graphic_character)
		printableFilterMap = { 
			'Lu': 1, 'Ll': 1, 'Lt': 1, 'Lm': 1, 'Lo': 1, 'Mn': 1, 'Mc': 1, 'Me': 1, 'Nd': 1, 'Nl': 1, 'No': 1,
			'Pc': 1, 'Pd': 1, 'Ps': 1, 'Pe': 1, 'Pi': 1, 'Pf': 1, 'Po': 1, 'Sm': 1, 'Sc': 1, 'Sk': 1, 'So': 1,
			'Zs': 2 
		}
		printableRanges = unicodeData.filter(2, lambda fs: printableFilterMap[fs[1]] if fs[1] in printableFilterMap else None)
		_enum: LookupType = LookupType.enumType('PrintableType', 'none', ['none', 'printable', 'printSpace'])
		_gen: CodeGen = file.next('Printable', 'Automatically generated from: Unicode General_Category is L*,M*,N*,P*,S* or optionally Zs')
		_gen.addEnum(_enum)
		_gen.intFunction('GetPrintable', _enum, printableRanges)

		# write the cased-enum to the file (https://www.unicode.org/reports/tr44/#Cased)
		caseFilterMap = { 'Lowercase': 1, 'Uppercase': 2 }
		caseRanges = derivedProperties.filter(1, lambda fs: caseFilterMap[fs[0]] if fs[0] in caseFilterMap else None)
		caseRanges = Ranges.union(caseRanges, unicodeData.filter(2, lambda fs: 3 if fs[1] == 'Lt' else None))
		_enum: LookupType = LookupType.enumType('CaseType', 'none', ['none', 'lowerCase', 'upperCase', 'titleCase'])
		_gen: CodeGen = file.next('Case', 'Automatically generated from: Unicode derived property Lowercase, Uppercase or General_Category Lt')
		_gen.addEnum(_enum)
		_gen.intFunction('GetCase', _enum, caseRanges)

		# write the category-enum to the file (https://www.unicode.org/reports/tr44/#GC_Values_Table)
		categoryEnumMap = {
			'Lu': 0, 'Ll': 1, 'Lt': 2, 'Lm': 3, 'Lo': 4, 'Mn': 5, 'Mc': 6, 'Me': 7, 'Nd': 8, 'Nl': 9, 'No': 10,
			'Pc': 11, 'Pd': 12, 'Ps': 13, 'Pe': 14, 'Pi': 15, 'Pf': 16, 'Po': 17, 'Sm': 18, 'Sc': 19, 'Sk': 20, 'So': 21,
			'Zs': 22, 'Zl': 23, 'Zp': 24, 'Cc': 25, 'Cf': 26, 'Cs': 27, 'Co': 28, 'Cn': 29
		}
		categoryRanges = unicodeData.filter(2, lambda fs: categoryEnumMap[fs[1]] if fs[1] in categoryEnumMap else None)
		_enum: LookupType = LookupType.enumType('CategoryType', 'cn', ['lu', 'll', 'lt', 'lm', 'lo', 'mn', 'mc', 'me', 'nd', 'nl', 'no', 'pc', 'pd', 'ps', 'pe', 'pi', 'pf', 'po', 'sm', 'sc', 'sk', 'so', 'zs', 'zl', 'zp', 'cc', 'cf', 'cs', 'co', 'cn'])
		_gen: CodeGen = file.next('Category', 'Automatically generated from: Unicode General_Category')
		_gen.addEnum(_enum)
		_gen.intFunction('GetCategory', _enum, categoryRanges)

# MapCase (encodes lowercase/uppercase/titlecase mappings)
def _ExpandSpecialCaseMap(conditions: dict[str, int], lowerFlag: int, upperFlag: int, titleFlag: int, lower: str, title: str, upper: str, condition: str) -> tuple[int]:
	# prepare the condition-string and ensure it is defined
	if condition == '':
		condition = 'none'
	condition = '_'.join(condition.split(' ')).lower()
	if condition not in conditions:
		conditions[condition] = len(conditions)

	# transform the strings to integers
	lowerValues: list[int] = ([] if lower == '' else [int(u, 16) for u in lower.split(' ')])
	upperValues: list[int] = ([] if upper == '' else [int(u, 16) for u in upper.split(' ')])
	titleValues: list[int] = ([] if title == '' else [int(u, 16) for u in title.split(' ')])

	# construct the output tuple as list of first the lower, then upper, then title values
	return tuple([conditions[condition] | lowerFlag, len(lowerValues)] + lowerValues
				+ [conditions[condition] | upperFlag, len(upperValues)] + upperValues
				+ [conditions[condition] | titleFlag, len(titleValues)] + titleValues)
def _TranslateSpecialCaseMap(cIndex: int, values: tuple[int], nullCondition: int, bitsOfValue: int) -> tuple[int]:
	index, out, nulls = 0, [], []

	# skip all conditions and only offset all actual values by the character and separate the null-conditions out to append them in the end
	while index < len(values):
		to = (nulls if (values[index] & bitsOfValue) == nullCondition else out)

		to.append(values[index])
		to.append(values[index + 1])
		for i in range(index + 2, index + 2 + values[index + 1]):
			to.append(values[i] - cIndex)
		index += 2 + values[index + 1]
	return tuple(out + nulls)
def _TranslateSimpleCaseValue(nullCondition: int, tpFlag: int, negativeFlag: int, bitsOfValue: int, value: int) -> tuple[int]:
	# check if the value can fit into a single cell
	if abs(value) <= bitsOfValue:
		return (abs(value) | tpFlag | (0 if value >= 0 else negativeFlag),)

	# add the null-condition and the entire value (negative-flag only necessary for single-cell values)
	return (nullCondition | tpFlag, 1, value)
def _HandleSpecialOrConflict(a: tuple[int], b: tuple[int]) -> tuple[int]:
	return (a[0] | b[0],) + a[1:]
def _TranslateCleanupCaseMap(values: tuple[int], lowerFlag: int, upperFlag: int, titleFlag: int, nullCondition: int, bitsOfValue: int) -> tuple[int]:
	if len(values) == 1:
		return values
	indices, modified = [0], [v for v in values]

	# setup the list of all indices of the starting conditions
	while True:
		next = indices[-1] + values[indices[-1] + 1] + 2
		if next >= len(values):
			break
		indices.append(next)

	# iterate over the conditions in reverse and clear all flags for conditions, for which the upcoming default already results in the same value
	lowerDef, upperDef, titleDef = [0], [0], [0]
	for i in range(len(indices) - 1, -1, -1):
		iValues = modified[indices[i] + 2:indices[i] + 2 + modified[indices[i] + 1]]

		# iterate over the flags and check if they can be removed from the current index
		for f, d in [(lowerFlag, lowerDef), (upperFlag, upperDef), (titleFlag, titleDef)]:
			if (modified[indices[i]] & f) == 0:
				continue
			if iValues == d:
				modified[indices[i]] ^= f

		# update the default values for all null-conditions and clear the current condition-bits for
		# all upcoming values, as this is a null-condition and will therefore be true at all times
		if (modified[indices[i]] & bitsOfValue) == nullCondition:
			for j in range(i + 1, len(indices)):
				modified[indices[j]] &= ~(modified[indices[i]] & (lowerFlag | upperFlag | titleFlag))
			if (modified[indices[i]] & lowerFlag) != 0:
				lowerDef = iValues
			if (modified[indices[i]] & upperFlag) != 0:
				upperDef = iValues
			if (modified[indices[i]] & titleFlag) != 0:
				titleDef = iValues

		# null the default-values as they will not be reached anymore
		else:
			if (modified[indices[i]] & lowerFlag) != 0:
				lowerDef = None
			if (modified[indices[i]] & upperFlag) != 0:
				upperDef = None
			if (modified[indices[i]] & titleFlag) != 0:
				titleDef = None

	# remove all indices, which have empty casing-flags, and merge neighboring similar types
	out, last, typeFlags = [], None, (lowerFlag | upperFlag | titleFlag)
	for index in indices:
		if (modified[index] & typeFlags) == 0:
			continue
		iValues = modified[index:index + 2 + modified[index + 1]]
		
		# check if the value is identical to the last value (except for the type-flags)
		if last is not None and out[last + 2:] == iValues[2:] and (out[last] & ~typeFlags) == (modified[index] & ~typeFlags):
			out[last] |= (modified[index] & typeFlags)
		else:
			last, out = len(out), out + iValues
	return tuple(out)
def _MergeCaseMap(a: tuple[int], b: tuple[int], nullCondition: int, typeFlags: int, negativeFlag: int, bitsOfValue: int) -> tuple[int]:
	# check if the values are the same and only differ by the type-flats
	if len(a) == 1 and len(b) == 1 and ((a[0] ^ b[0]) & ~typeFlags) == 0:
		return (a[0] | b[0],)

	# check if any of the values is a single cell, in which case it must be expanded to the null-condition
	if len(a) == 1:
		a = (nullCondition | (a[0] & ~(bitsOfValue | negativeFlag)), 1, -(a[0] & bitsOfValue) if (a[0] & negativeFlag) else (a[0] & bitsOfValue))
	if len(b) == 1:
		b = (nullCondition | (b[0] & ~(bitsOfValue | negativeFlag)), 1, -(b[0] & bitsOfValue) if (b[0] & negativeFlag) else (b[0] & bitsOfValue))
	return a + b
def MakeCodepointMaps(config: GenerateConfig):
	# parse the relevant files
	unicodeData = ParsedFile('ucd/UnicodeData.txt', True)
	specialCasing = ParsedFile('ucd/SpecialCasing.txt', False)
	derivedProperties = ParsedFile('ucd/DerivedCoreProperties.txt', False)
	propList = ParsedFile('ucd/PropList.txt', False)

	# write all map functions to the file
	with GeneratedFile('unicode-cp-maps.h', config) as file:
		# define the flags used for the separate values (keep the topmost bit clear as value is signed)
		flagIsNegative = 0x4000_0000
		flagIsCased = 0x2000_0000
		flagIsIgnorable = 0x1000_0000
		flagIsSoftDotted = 0x0800_0000
		flagCombClass0or230 = 0x0400_0000
		flagCombClass230 = 0x0200_0000
		flagIsLower = 0x0100_0000
		flagIsUpper = 0x0080_0000
		flagIsTitle = 0x0040_0000
		flagIs0049 = 0x0020_0000
		flagIs0307 = 0x0010_0000
		bitsOfValue = 0x000f_ffff

		# extract all attributes and merge them together
		propertyMask = derivedProperties.filter(1, lambda fs: flagIsCased if fs[0] == 'Cased' else None)
		propertyMask = Ranges.merge(propertyMask, derivedProperties.filter(1, lambda fs: flagIsIgnorable if fs[0] == 'Case_Ignorable' else None), _HandleSpecialOrConflict)
		propertyMask = Ranges.merge(propertyMask, propList.filter(1, lambda fs: flagIsSoftDotted if fs[0] == 'Soft_Dotted' else None), _HandleSpecialOrConflict)
		propertyMask = Ranges.merge(propertyMask, unicodeData.filter(3, lambda fs: flagCombClass0or230 if (fs[2] == '0' or fs[2] == '230') else None), _HandleSpecialOrConflict)
		propertyMask = Ranges.merge(propertyMask, unicodeData.filter(3, lambda fs: flagCombClass230 if fs[2] == '230' else None), _HandleSpecialOrConflict)
		propertyMask = Ranges.merge(propertyMask, [Range(0x0049, 0x0049, flagIs0049), Range(0x0307, 0x0307, flagIs0307)], _HandleSpecialOrConflict)

		# extract all special casing rules (as 'sorted' is stable, values are guaranteed to be ordered by appearance)
		caseConditions = { 'none': 0 }
		specialRanges = specialCasing.filter(4, lambda fs: _ExpandSpecialCaseMap(caseConditions, flagIsLower, flagIsUpper, flagIsTitle, fs[0], fs[1], fs[2], fs[3]), True, lambda a, b: _MergeCaseMap(a, b, 0, 0, 0, 0))

		# sanitize and cleanup the special ranges, as all none-conditions must lie in the end, as conditions are more relevant, if they match
		specialRanges = Ranges.translate(specialRanges, lambda c, v: _TranslateSpecialCaseMap(c, v, caseConditions['none'], bitsOfValue))
		caseConditions['_last'] = len(caseConditions)
		if len(caseConditions) > bitsOfValue:
			raise RuntimeError('Conditions overflow values')

		# extract all simple lower-case mappings
		simpleLowerRanges = unicodeData.filter(14, lambda fs: int(fs[12], 16) if fs[12] != '' else None)
		simpleLowerRanges = Ranges.translate(simpleLowerRanges, lambda c, v: _TranslateSimpleCaseValue(caseConditions['none'], flagIsLower, flagIsNegative, bitsOfValue, v[0] - c))

		# extract all simple upper-case mappings
		simpleUpperRanges = unicodeData.filter(14, lambda fs: int(fs[11], 16) if fs[11] != '' else None)
		simpleUpperRanges = Ranges.translate(simpleUpperRanges, lambda c, v: _TranslateSimpleCaseValue(caseConditions['none'], flagIsUpper, flagIsNegative, bitsOfValue, v[0] - c))

		# extract all simple title-case mappings
		simpleTitleRanges = unicodeData.filter(14, lambda fs: int(fs[13], 16) if fs[13] != '' else None)
		simpleTitleRanges = Ranges.translate(simpleTitleRanges, lambda c, v: _TranslateSimpleCaseValue(caseConditions['none'], flagIsTitle, flagIsNegative, bitsOfValue, v[0] - c))

		# merge the simple values together
		simpleRanges = Ranges.merge(simpleLowerRanges, simpleUpperRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, bitsOfValue))
		simpleRanges = Ranges.merge(simpleRanges, simpleTitleRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, bitsOfValue))

		# merge the special ranges and simple ranges together (special-ranges before simple-ranges to preserve order in
		# reconstructed final ranges) and clean the ranges up to remove irrelevant conditions and combine the properties into it
		caseRanges = Ranges.merge(specialRanges, simpleRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, bitsOfValue))
		caseRanges = Ranges.translate(caseRanges, lambda _, v: _TranslateCleanupCaseMap(v, flagIsLower, flagIsUpper, flagIsTitle, caseConditions['none'], bitsOfValue))
		caseRanges = Ranges.merge(caseRanges, propertyMask, _HandleSpecialOrConflict)

		# write the case-mapping and enum to the file (https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf, https://www.unicode.org/reports/tr44/#Casemapping)
		caseType: LookupType = LookupType.intType(0, 'int32_t')
		_gen: CodeGen = file.next('MapCase', 'Automatically generated from: Special-Casing, unicode-data simple case mapping')
		_gen.addConstInt(caseType, 'FlagIsNegative', flagIsNegative)
		_gen.addConstInt(caseType, 'FlagIsCased', flagIsCased)
		_gen.addConstInt(caseType, 'FlagIsIgnorable', flagIsIgnorable)
		_gen.addConstInt(caseType, 'FlagIsSoftDotted', flagIsSoftDotted)
		_gen.addConstInt(caseType, 'FlagCombClass0or230', flagCombClass0or230)
		_gen.addConstInt(caseType, 'FlagCombClass230', flagCombClass230)
		_gen.addConstInt(caseType, 'FlagIsLower', flagIsLower)
		_gen.addConstInt(caseType, 'FlagIsUpper', flagIsUpper)
		_gen.addConstInt(caseType, 'FlagIsTitle', flagIsTitle)
		_gen.addConstInt(caseType, 'FlagIs0049', flagIs0049)
		_gen.addConstInt(caseType, 'FlagIs0307', flagIs0307)
		_gen.addConstInt(caseType, 'BitsOfPayload', bitsOfValue)
		_gen.addEnum(LookupType.enumType('CaseCond', 'none', caseConditions))
		_gen.listFunction('MapCase', LookupType.intType(0, 'int32_t'), caseRanges)

# LookupWordType
def MakeCodepointAnalysis(config: GenerateConfig):
	# parse the relevant files
	wordBreak = ParsedFile('ucd/WordBreakProperty.txt', False)
	emojiData = ParsedFile('ucd/EmojiData.txt', False)

	# write all maps functions to the file
	with GeneratedFile('unicode-cp-analysis.h', config) as file:
		# setup the word-break boundary ranges
		flagIsPictographic = 0x80
		wordEnumMap = { 'Other': 0, 'CR': 1, 'LF': 2, 'Newline': 3, 'Extend': 4, 'ZWJ': 5, 'Regional_Indicator': 6, 'Format': 7,
			'Katakana': 8, 'Hebrew_Letter': 9, 'ALetter': 10, 'Single_Quote': 11, 'Double_Quote': 12, 'MidNumLet': 13, 'MidLetter': 14,
			'MidNum': 15, 'Numeric': 16, 'ExtendNumLet': 17, 'WSegSpace': 18 }
		wordRanges, wordRangesDef = wordBreak.extractAll(1, 0, wordEnumMap)
		if wordRangesDef != wordEnumMap['Other']:
			raise RuntimeError('Default break-value is expected to be [other]')
		if len(wordEnumMap) >= flagIsPictographic:
			raise RuntimeError('Flags too small for word-break enum')
		wordRanges = Ranges.merge(wordRanges, emojiData.filter(1, lambda fs: flagIsPictographic if fs[0] == 'Extended_Pictographic' else None), lambda a, b: (a[0] | b[0],))

		# generate the enum, extended_pictographic flag, and the lookup function (https://unicode.org/reports/tr29/#Word_Boundaries)
		_enum: LookupType = LookupType.enumType('WordType', 'other', ['other', 'cr', 'lf', 'newline', 'extend', 'zwj', 'regional_indicator', 'format', 'katakana', 'hebrew_letter', 'a_letter', 'single_quote', 'double_quote', 'mid_num_letter', 'mid_letter', 'mid_num', 'numeric', 'extend_num_let', 'w_seg_space', '_last'])
		_type: LookupType = LookupType.intType(0, 'uint8_t')
		_gen: CodeGen = file.next('Word', 'Automatically generated from: Unicode WordBreakProperty and EmojiData')
		_gen.addConstInt(_type, 'FlagIsPictographic', flagIsPictographic)
		_gen.addEnum(_enum)
		_gen.intFunction('LookupWordType', _type, wordRanges)
	

# check if the files need to be downloaded and extract the version and date-time
generatedURLOrigin = 'https://www.unicode.org/Public/UCD/latest'
generatedVersion = DownloadUCDFiles('--refresh' in sys.argv, generatedURLOrigin)
generatedDateTime = datetime.datetime.today().strftime('%Y-%m-%d %H:%M')
generatedConfig = GenerateConfig(generatedURLOrigin, generatedVersion, generatedDateTime, ['cinttypes', 'utility', 'algorithm'])

# generate the actual files
MakeCodepointQuery(generatedConfig)
MakeCodepointMaps(generatedConfig)
MakeCodepointAnalysis(generatedConfig)




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

# MakeGraphemeTypeMapping()
