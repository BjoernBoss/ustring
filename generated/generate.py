# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2024 Bjoern Boss Henrichsen
import urllib.request
import os
import sys
import datetime
import re

# added to all generated files
copyrightLines = [
	'/* SPDX-License-Identifier: BSD-3-Clause */',
	f'/* Copyright (c) {datetime.datetime.now().year} Bjoern Boss Henrichsen */'
]

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
	def neighbors(self, right: 'Range') -> bool:
		return (self.last + 1 == right.first)
	def overlap(self, other: 'Range') -> bool:
		return (self.last >= other.first and self.first <= other.last)
	def distant(self, right: 'Range') -> bool:
		return (self.last + 1 < right.first)

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
			Ranges._appOrMerge(out, leading)

		# iterate over the conflicting range and process it
		otherRemaining: Range|None = other
		while len(conflict) > 0 and otherRemaining is not None:
			# extract the left and right conflicting range
			if conflict[0].span() == otherRemaining.span():
				left, right, conflict, otherRemaining = conflict[0], otherRemaining, [], None
			elif conflict[0].span() < otherRemaining.span():
				left, conflict = conflict[0], conflict[1:]
				right, otherRemaining = otherRemaining.split(left.span())
			else:
				left, conflict[0] = conflict[0].split(otherRemaining.span())
				right, otherRemaining = otherRemaining, None

			# check if the conflict is trivial or left the handler process it
			if left.values == right.values:
				Ranges._appOrMerge(out, left.merge(right))
			else:
				Ranges._appOrMerge(out, Range(left.first, left.last, conflictHandler(left.values, right.values)))

		# append the remainder to the output
		for c in conflict:
			Ranges._appOrMerge(out, c)
		if otherRemaining is not None:
			Ranges._appOrMerge(out, otherRemaining)
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
	def _modifyOperation(a: tuple[int]|None, b: tuple[int]|None, conflictHandler) -> tuple[int]|None:
		if a is None:
			return None
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
	def lookup(ranges: list[Range], pos: int) -> tuple[int]|None:
		for r in ranges:
			if pos >= r.first and pos <= r.last:
				return r.values
		return None

	@staticmethod
	def modify(base: list[Range], mask: list[Range], conflictHandler) -> list[Range]:
		return Ranges._setIteration(base, mask, False, lambda a, b: Ranges._modifyOperation(a, b, conflictHandler))
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
				value = assignValue(i, r.values)
				if value is not None:
					Ranges._appOrMerge(out, Range(i, i, value))
		return out

class CodeGenIndirect:
	def __init__(self, nestDepth: int = 3, minBufSize: int = 128, shiftTests: list[int] = [0, 1, 2, 3, 4, 5, 6, 7]) -> None:
		self._nestingDepth = nestDepth
		self._minBufferSize = minBufSize
		self._shiftTests = shiftTests
	@property
	def nestingDepth(self) -> int:
		return self._nestingDepth
	@property
	def minBufferSize(self) -> int:
		return self._minBufferSize
	@property
	def shiftTests(self) -> int:
		return self._shiftTests

class CodeGenDensityIfElse:
	def __init__(self, lookupDensity: float = 1/3, minClusterSize: int = 8, preferAscii: bool = True) -> None:
		self._lookupDensity = lookupDensity
		self._minClusterSize = minClusterSize
		self._preferAscii = preferAscii
	@property
	def lookupDensity(self) -> float:
		return self._lookupDensity
	@property
	def minClusterSize(self) -> int:
		return self._minClusterSize
	@property
	def preferAscii(self) -> int:
		return self._preferAscii

class CodeGenBinarySearch:
	def __init__(self, maxValueSize: int = 0x10000, nestDepth: int = 2, minBufSize: int = 128, shiftTests: list[int] = [0, 1, 2, 3, 4, 5, 6, 7]) -> None:
		self._maxValueSize = maxValueSize
		self._nestingDepth = nestDepth
		self._minBufferSize = minBufSize
		self._shiftTests = shiftTests
	@property
	def maxValueSize(self) -> int:
		return self._maxValueSize
	@property
	def nestingDepth(self) -> int:
		return self._nestingDepth
	@property
	def minBufferSize(self) -> int:
		return self._minBufferSize
	@property
	def shiftTests(self) -> int:
		return self._shiftTests

type CodeGenOpts = CodeGenIndirect|CodeGenDensityIfElse|CodeGenBinarySearch
class CodeGenConfig:
	def __init__(self, lookup: CodeGenOpts, asciiLookup: CodeGenOpts|None = None, listConsideredSparse: float = 0.3) -> None:
		self._lookup = lookup
		self._ascii = asciiLookup
		self._listConsideredSparse = listConsideredSparse
	@property
	def lookup(self) -> CodeGenOpts:
		return self._lookup
	@property
	def ascii(self) -> CodeGenOpts|None:
		return self._ascii
	@property
	def listConsideredSparse(self) -> float:
		return self._listConsideredSparse

class CodeGen:
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
			btFormat = (tp.bufferSize() == 1)
			wdFormat = (tp.bufferSize() == 2)

			# write the header, data, and trailer out
			self._file.write(f'static constexpr {tp.bufferType()} {name}[{len(data)}] = {{\n\t')
			for i in range(len(data)):
				if i > 0:
					self._file.write(f',{"\n\t" if (i % valsPerLine) == 0 else ""}')
				if btFormat:
					self._file.write(f' {data[i]: #05x}')
				elif wdFormat:
					self._file.write(f' {data[i]: #07x}')
				else:
					self._file.write(f' {data[i]: #011x}')
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

	def _binarySearchLookup(self, data: list[int], tp: LookupType, inVarName: str, outWriteTo: str, hint: str, bufMaxNesting: int, bufMinIndSize: int, bufShiftTests: list[int]) -> str:
		self._pushContext(tp, inVarName, outWriteTo, hint)
		self._indirectBuffer(self._rangeFromList(data), bufMaxNesting, bufMinIndSize, bufShiftTests)
		return self._popContext()
	def _binarySearch(self, ranges: list[Range], maxBinSearchSize: int, bufMaxNesting: int, bufMinIndSize: int, bufShiftTests: list[int]) -> None:
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
			getValueCode = self._binarySearchLookup([r.values[0] for r in ranges], self._type, leftVarName, self._outTo, 'Value', bufMaxNesting, bufMinIndSize, bufShiftTests)
			if StrHelp.multiLines(getValueCode):
				getValueCode = f' {{\n{StrHelp.indent(getValueCode)}}}\n'
			else:
				getValueCode = f'\n\t{getValueCode}'
		else:
			getValueCode = f'\n\t{self._outTo} {self._type.staticLookup(ranges[0].values[0])};\n'

		# create the lookup code for the ranges sizes (store the size smaller by 1 as a size cannot be 0)
		sizeBufferList = [r.span() - 1 for r in ranges]
		sizeBufferType = LookupType.listType(-1, sizeBufferList)
		getSizeCode = self._binarySearchLookup(sizeBufferList, sizeBufferType, leftVarName, f'{sizeVarName} =', 'Size', bufMaxNesting, bufMinIndSize, bufShiftTests)

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

	def _ifElseClusters(self, ranges: list[Range], densityThreshold: float, minClusterSize: int, preferAscii: bool) -> None:
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

			# reorganize ascii-ranges before the remainder or simply sort all ranges by size of their span
			for i in range(2 if preferAscii else 1):
				rangesToSort = ([a for a in actual if (i == 0) == (a.first < 0x80)] if preferAscii else actual)

				# extract all ranges which contain more than two chars
				sortedActual += sorted([a for a in rangesToSort if a.values[0] > 2], key=lambda x: -x.values[0])

				# add all remaining single checks, but sorted in ascending order
				restToSort = [a for a in rangesToSort if a.values[0] == 1]
				restToSort += [Range(a.first, a.first, 1) for a in rangesToSort if a.values[0] == 2]
				restToSort += [Range(a.last, a.last, 1) for a in rangesToSort if a.values[0] == 2]
				sortedActual += sorted(restToSort, key=lambda x: x.first)

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
	def _indirectBuffer(self, ranges: list[Range], nestingLevel: int, minIndirectSize: int, shiftList: list[int]) -> None:
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

		# setup the size-based buffers as well as the mapping to the corresponding slots
		indexMap: dict[int|None, tuple[int, int]] = {}
		bufferData: dict[int, list[int]] = {}
		for i in range(len(indirections)):
			_, datBuffer, idxBuffer = indirections[i]

			# write the data to the buffer
			datSize: int = LookupType.listType(0, datBuffer).bufferSize()
			if datSize not in bufferData:
				bufferData[datSize] = []
			indexMap[i] = (len(bufferData[datSize]), datSize)
			bufferData[datSize] += datBuffer

			# write the index to the buffer (there can only exist one index-buffer)
			if len(idxBuffer) == 0:
				continue
			datSize = LookupType.listType(0, idxBuffer).bufferSize()
			if datSize not in bufferData:
				bufferData[datSize] = []
			indexMap[None] = (len(bufferData[datSize]), datSize)
			bufferData[datSize] += idxBuffer

		# check if the buffer(s) need to be allocated
		bufferName: dict[int, str] = {}
		for k in bufferData:
			bufferName[k] = self._addBuffer(bufferData[k], LookupType.listType(0, bufferData[k]), f'IndData{k}')

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
			indexLookupCode = f'{list[bufferName.values()][0]}[{inVarName}]'
			self._code += f'{indent}{self._outTo} {self._type.dynLookup(indexLookupCode)};\n'
			if lowerClipped or upperClipped:
				self._code += '}\n'
			return

		# add the initial (and only) index lookup, as all other indices will be computed and looked up from the data-buffer
		indexVarName = self._addVar('Index')
		indexLookupCode = f'{inVarName} >> {shiftSum}'
		if indexMap[None][0] > 0:
			indexLookupCode = f'{self._value(indexMap[None][0])} + ({indexLookupCode})'
		self._code += f'{indent}size_t {indexVarName} = {bufferName[indexMap[None][1]]}[{indexLookupCode}];\n'

		# iterate over the indirections and apply them to the index-variable
		for i in range(len(indirections) - 1, -1, -1):
			shift = indirections[i][0]
			dataOffset, bufNameIndex = indexMap[i]

			# add the operation to compute the index into the data-buffer for the current indirection
			shiftSum -= shift
			indexLookupCode = (f'({inVarName} >> {shiftSum})' if shiftSum > 0 else inVarName)
			indexLookupCode = f'({indexVarName} << {shift}) + ({indexLookupCode} & {(1 << shift) - 1:#04x})'

			# add the memory lookup (i.e. the 'data')
			if dataOffset > 0:
				indexLookupCode = f'{bufferName[bufNameIndex]}[{indexLookupCode} + {dataOffset:#04x}]'
			else:
				indexLookupCode = f'{bufferName[bufNameIndex]}[{indexLookupCode}]'

			# add the line to the produced code, as well as the final dynamic lookup
			self._code += f'{indent}{indexVarName} = {indexLookupCode};\n'
			if i == 0:
				self._code += f'{indent}{self._outTo} {self._type.dynLookup(indexVarName)};\n'

		# add the closing indentation
		if lowerClipped or upperClipped:
			self._code += '}\n'

	def _lookupRanges(self, ranges: list[Range], tp: LookupType, addIndentation: bool, inVarName: str, outWriteTo: str, hint: str, config: CodeGenOpts) -> str:
		# check if the range is empty or trivial and nothing needs to be done
		if len(ranges) == 0:
			return f'{"\n\t" if addIndentation else ""}{outWriteTo} {tp.staticLookup(tp.defValue())};\n'
		if len(ranges) == 1:
			return f'{"\n\t" if addIndentation else ""}{outWriteTo} {tp.staticLookup(ranges[0].values[0])};\n'

		# setup the new context for the execution
		self._pushContext(tp, inVarName, outWriteTo, hint)

		# check if a code-lookup should be performed
		if type(config) == CodeGenDensityIfElse:
			self._ifElseClusters(ranges, config.lookupDensity, config.minClusterSize, config.preferAscii)

		# check if a lookup using indirect buffers should be performed
		elif type(config) == CodeGenIndirect:
			self._indirectBuffer(ranges, config.nestingDepth, config.minBufferSize, config.shiftTests)

		# perform the binary search
		else:
			self._binarySearch(ranges, config.maxValueSize, config.nestingDepth, config.minBufferSize, config.shiftTests)

		# fetch the produced code
		lookupCode: str = self._popContext()

		# check if brackets need to be added
		if addIndentation:
			if StrHelp.multiLines(lookupCode):
				lookupCode = ' {\n' + StrHelp.indent(lookupCode) + '}\n'
			else:
				lookupCode = '\n\t' + lookupCode
		return lookupCode
	def _fullRangeLookup(self, ranges: list[Range], tp: LookupType, inVarName: str, outVarName: str|None, config: CodeGenConfig) -> str:
		# initialize the out variable-name
		if outVarName is None:
			outVarName = 'return'
		else:
			outVarName += ' ='
		lookupCode = ''

		# check if the entire ranges should be handled as a single lookup
		if config.ascii is None or ranges[-1].first <= 0x80:
			return self._lookupRanges(ranges, tp, False, inVarName, outVarName, '', config.lookup)

		# add the ascii lookup
		lookupCode += f'if ({inVarName} < 0x80)'
		asciiRanges, upperRanges = Ranges.split(ranges, 0x80)
		lookupCode += self._lookupRanges(asciiRanges, tp, True, inVarName, outVarName, 'Ascii', config.ascii)

		# add the remaining lookup code
		lookupCode += 'else'
		lookupCode += self._lookupRanges(upperRanges, tp, True, inVarName, outVarName, '', config.lookup)
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
	def intFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range], config: CodeGenConfig) -> None:
		print(f'Creating integer-lookup {fnName}...')
		Ranges.wellFormed(ranges, lookupType)

		# fill all holes compared to the entire valid range with the default value
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# generate the lookup-code and write the final buffers out
		lookupCode: str = self._fullRangeLookup(ranges, lookupType, 'cp', None, config)
		self._writeBuffers()

		# generate the actual function and insert the final code
		self._file.writeln(f'inline constexpr {lookupType.typeName()} {fnName}(char32_t cp) {{')
		self._file.write(StrHelp.indent(lookupCode))
		self._file.writeln('}')
	def listFunction(self, fnName: str, lookupType: LookupType, ranges: list[Range], dataOnly: bool, config: CodeGenConfig) -> None:
		print(f'Creating list-lookup {fnName}...')
		Ranges.wellFormed(ranges, lookupType)

		# fill the entire valid range with the default value (must be done before setting up the
		#	data-buffer as the default-value will otherwise not be written to the data-buffer)
		ranges = Ranges.fill(ranges, lookupType.defValue(), Range.RangeFirst, Range.RangeLast)

		# check if a sparse buffer should be created for the map (data-buffer contains either single value, or null
		#	followed by size followed by values; null-value also needs special encoding) or simply write size, and data out
		sparseBuffer = not dataOnly and (sum(1 for r in ranges if len(r.values) > 1) <= len(ranges) * config.listConsideredSparse)

		# create the data buffer for the ranges and remap the ranges to indices into the data-buffer
		dataBuffer, dataIndex = [], {}
		for i in range(len(ranges)):
			values = ranges[i].values

			# check if the value needs to be added to the buffer
			if values not in dataIndex:
				dataIndex[values] = len(dataBuffer)
				if dataOnly:
					dataBuffer += [v for v in values]
				elif not sparseBuffer:
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
		lookupCode: str = self._fullRangeLookup(ranges, LookupType.rangeType(0, 0, len(dataIndex) - 1), 'cp', indexVarName, config)
		self._writeBuffers()

		# generate the actual function and insert the generated code
		if dataOnly:
			self._file.writeln(f'inline constexpr const {lookupType.typeName()}* {fnName}(char32_t cp) {{')
		else:
			self._file.writeln(f'inline constexpr std::pair<size_t, const {lookupType.typeName()}*> {fnName}(char32_t cp) {{')
		self._file.writeln(f'\tsize_t {indexVarName} = 0;')
		self._file.write(StrHelp.indent(lookupCode))

		# add the logic to extract the size and data
		if dataOnly:
			self._file.writeln(f'\treturn {dataBufferName} + {indexVarName};')
		elif sparseBuffer:
			self._file.writeln(f'\tif ({dataBufferName}[{indexVarName}] != 0)')
			self._file.writeln(f'\t\treturn {{ 1, {dataBufferName} + {indexVarName} }};')
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName} + 1]), {dataBufferName} + {indexVarName} + 2 }};')
		else:
			self._file.writeln(f'\treturn {{ size_t({dataBufferName}[{indexVarName}]), {dataBufferName} + {indexVarName} + 1 }};')
		self._file.writeln('}')

class SystemConfig:
	def __init__(self, url: str, version: str, date: str, mapping: dict[str, str], includes: list[str]) -> None:
		self.url = url
		self.version = version
		self.date = date
		self.includes = includes
		self.mapping = mapping

class GeneratedFile:
	def __init__(self, path: str, config: SystemConfig) -> None:
		self._path = path
		self._config = config
		self._file = None
		self._hadFirstBlock = False
		self._atStartOfLine = False
		self._indented = False
	def __enter__(self) -> 'GeneratedFile':
		self._file = open(self._path, mode='w', encoding='ascii')
		self._atStartOfLine = True

		# add the copy-right header
		for line in copyrightLines:
			self.writeln(line)

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
	def singleMissing(self, assignValue) -> tuple[list[Range], tuple[int]]:
		defMissing, valRanges = None, []

		# iterate over the lines and construct the missing and value-set
		for (begin, last, missing, fields) in self._parsed:
			value = assignValue(fields)
			if value is None:
				continue
			r = Range(begin, last, value)

			# check if this is the first overall missing-value and otherwise add it to the proper range
			if not missing:
				valRanges.append(r)
			elif defMissing is None:
				defMissing = r
			else:
				raise RuntimeError('Multiple default values encountered')
		valRanges = Ranges.fromRawList(valRanges)

		# check that the default-missing range covers the entire range
		valFirst, valLast = ((Range.RangeLast, Range.RangeFirst) if len(valRanges) == 0 else (valRanges[0].first, valRanges[-1].last))
		if defMissing is None or defMissing.first > valFirst or defMissing.last < valLast:
			raise RuntimeError('Invalid default-missing value')
		return valRanges, defMissing.values
	def multiMissing(self, assignValue) -> tuple[list[Range], tuple[int]]:
		defMissing, misRanges, valRanges = None, [], []

		# iterate over the lines and construct the missing and value-set
		for (begin, last, missing, fields) in self._parsed:
			value = assignValue(fields)
			if value is None:
				continue
			r = Range(begin, last, value)

			# check if this is the first overall missing-value and otherwise add it to the proper range
			if not missing:
				valRanges.append(r)
			elif defMissing is None:
				defMissing = r
			else:
				misRanges.append(r)
		misRanges = Ranges.fromRawList(misRanges)
		valRanges = Ranges.fromRawList(valRanges)

		# check that the default-missing range covers the entire other two ranges
		misFirst, misLast = ((Range.RangeLast, Range.RangeFirst) if len(misRanges) == 0 else (misRanges[0].first, misRanges[-1].last))
		valFirst, valLast = ((Range.RangeLast, Range.RangeFirst) if len(valRanges) == 0 else (valRanges[0].first, valRanges[-1].last))
		if defMissing is None or defMissing.first > min(misFirst, valFirst) or defMissing.last < max(misLast, valLast):
			raise RuntimeError('Invalid default-missing value')
		return Ranges.merge(misRanges, valRanges, lambda _, b: b), defMissing.values
	def values(self, assignValue, ignoreMissing: bool = False) -> list[Range]:
		ranges: list[Range] = []

		# iterate over the parsed lines and match them against the callback
		for (begin, last, missing, fields) in self._parsed:
			if missing and ignoreMissing:
				continue
			value = assignValue(fields)
			if value is None:
				continue
			if not missing:
				ranges.append(Range(begin, last, value))
			else:
				raise RuntimeError('Unexpected missing default-value')
		return Ranges.fromRawList(ranges)
	def conflicting(self, assignValue, conflictHandler) -> list[Range]:
		ranges: list[Range] = []

		# iterate over the parsed lines and match them against the callback
		for (begin, last, missing, fields) in self._parsed:
			if missing:
				raise RuntimeError('Unexpected missing default-value')
			value = assignValue(fields)
			if value is not None:
				ranges.append(Range(begin, last, value))
		return Ranges.fromConflictingRawList(ranges, conflictHandler)

# download all relevant files from the latest release of the ucd and extract the version (unicode character database: https://www.unicode.org/Public/UCD/latest)
def DownloadUCDFiles(refreshFiles: bool, includeMain: bool, includeTest: bool, baseUrl: str) -> tuple[str, dict[str, str], str]:
	files = {
		'ReadMe': 'ReadMe.txt'
	}
	dirPath = './ucd'
	testPath = './tests'
	if includeMain:
		files['UnicodeData'] = 'ucd/UnicodeData.txt'
		files['PropList'] = 'ucd/PropList.txt'
		files['DerivedCoreProperties'] = 'ucd/DerivedCoreProperties.txt'
		files['SpecialCasing'] = 'ucd/SpecialCasing.txt'
		files['WordBreakProperty'] = 'ucd/auxiliary/WordBreakProperty.txt'
		files['EmojiData'] = 'ucd/emoji/emoji-data.txt'
		files['EastAsianWidth'] = 'ucd/EastAsianWidth.txt'
		files['GraphemeBreakProperty'] = 'ucd/auxiliary/GraphemeBreakProperty.txt'
		files['SentenceBreakProperty'] = 'ucd/auxiliary/SentenceBreakProperty.txt'
		files['LineBreak'] = 'ucd/LineBreak.txt'
		files['CaseFolding'] = 'ucd/CaseFolding.txt'
		files['DerivedNormalizationProps'] = 'ucd/DerivedNormalizationProps.txt'
		files['NormalizationCorrections'] = 'ucd/NormalizationCorrections.txt'
		files['DerivedBidiClass'] = 'ucd/extracted/DerivedBidiClass.txt'
	if includeTest:
		files['WordBreakTest'] = 'ucd/auxiliary/WordBreakTest.txt'
		files['SentenceBreakTest'] = 'ucd/auxiliary/SentenceBreakTest.txt'
		files['GraphemeBreakTest'] = 'ucd/auxiliary/GraphemeBreakTest.txt'
		files['LineBreakTest'] = 'ucd/auxiliary/LineBreakTest.txt'
		files['NormalizationTest'] = 'ucd/NormalizationTest.txt'
		files['emoji-test'] = '../../emoji/latest/emoji-test.txt'

	# check if the directory needs to be created
	if not os.path.isdir(dirPath):
		os.mkdir(dirPath)
	if includeTest and not os.path.isdir(testPath):
		os.mkdir(testPath)

	# download all of the files (only if they should either be refreshed, or do not exist yet)
	mapping = {}
	for file in files:
		url, path = f'{baseUrl}/{files[file]}', f'{dirPath}/{file}.txt'
		mapping[file] = path

		if not refreshFiles and os.path.isfile(path):
			continue
		print(f'downloading [{url}] to [{path}]...')
		urllib.request.urlretrieve(url, path)

	# fetch the version from the read-me
	with open('ucd/ReadMe.txt', 'r') as f:
		fileContent = f.read()
	version = re.findall('Version ([0-9]+(\\.[0-9]+)*) of the Unicode Standard', fileContent)
	if len(version) != 1:
		raise RuntimeError('Unable to extract the version')
	return (version[0][0], mapping, f'{testPath}/')

# parse test-file for separators/normalization and create source-code test file
def CreateSeparatorTestFile(outPath: str, inPath: str, name: str) -> None:
	print(f'Creating [{outPath}] from [{inPath}] for test [{name}]...')
	tests: list[tuple[str, list[tuple[int, int]]]] = []

	# open the file which contains the test-sequences and parse it
	with open(inPath, 'r', encoding='utf-8') as file:
		for line in file:
			# remove any comments and skip empty lines
			line = line.split('#')[0].strip()
			if len(line) == 0:
				continue
			words = [w.strip() for w in line[1:-1].split('÷')]

			# create the combined string and ranges
			string, ranges = '', []
			for word in words:
				chars = [int(c.strip(), 16) for c in word.split('×')]
				start = len(string) // 10
				ranges.append((start, start + len(chars) - 1))
				for chr in chars:
					string += f'\\U{chr:08x}'

			# add the string and ranges to the tests
			tests.append((string, ranges))

	# open the file to contain the testing code and write it to the file
	with open(outPath, 'w', encoding='utf-8') as file:
		file.write('#pragma once\n')
		file.write('\n')
		file.write('#include <vector>\n')
		file.write('#include <utility>\n')
		file.write('\n')
		file.write('namespace test {\n')
		file.write(f'\tstatic constexpr size_t {name}Count = {len(tests)};\n')
		file.write('\n')

		# write all strings to the file
		file.write(f'\tstatic const char32_t* {name}Words[test::{name}Count] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'U\"{tests[i][0]}\"')
		file.write('\n\t};\n')
		file.write('\n')

		# write all range offsets to the file
		file.write(f'\tstatic std::pair<size_t, size_t> {name}RangesIndex[test::{name}Count] = {{')
		offset = 0
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'{{ {offset}, {len(tests[i][1])} }}')
			offset += len(tests[i][1]) * 2
		file.write('\n\t};\n')
		file.write('\n')

		# write the range blob to the file
		file.write(f'\tstatic size_t {name}RangesBlob[{offset}] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			for j in range(len(tests[i][1])):
				file.write('' if j == 0 else ', ')
				file.write(f'{tests[i][1][j][0]}, {tests[i][1][j][1]}')
		file.write('\n\t};\n')
		file.write('}\n')
def CreateNormalizationTestFile(outPath: str, inPath: str) -> None:
	print(f'Creating [{outPath}] from [{inPath}] for test [normalization]...')
	tests: list[tuple[str, str, str]] = []

	# open the file which contains the test-sequences and parse it
	with open(inPath, 'r', encoding='utf-8') as file:
		for line in file:
			# remove any comments and skip empty lines
			line = line.split('#')[0].strip()
			if len(line) == 0 or line.startswith('@Part'):
				continue
			if line.endswith(';'):
				line = line[:-1]
			words = [[int(c.strip(), 16) for c in w.strip().split(' ')] for w in line.split(';')]
			words = [''.join([f'\\U{i:08x}' for i in w]) for w in words]

			# add the string to the tests (only source, nfc, nfd)
			tests.append((words[0], words[1], words[2]))

	# open the file to contain the testing code and write it to the file
	with open(outPath, 'w', encoding='utf-8') as file:
		file.write('#pragma once\n')
		file.write('\n')
		file.write('#include <cinttypes>\n')
		file.write('\n')
		file.write('namespace test {\n')
		file.write(f'\tstatic constexpr size_t NormalizationCount = {len(tests)};\n')
		file.write('\n')

		# write all source-strings to the file
		file.write(f'\tstatic const char32_t* NormalizationSource[test::NormalizationCount] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'U\"{tests[i][0]}\"')
		file.write('\n\t};\n')
		file.write('\n')

		# write all nfc-strings to the file
		file.write(f'\tstatic const char32_t* NormalizationComposed[test::NormalizationCount] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'U\"{tests[i][1]}\"')
		file.write('\n\t};\n')
		file.write('\n')

		# write all nfd-strings to the file
		file.write(f'\tstatic const char32_t* NormalizationDecomposed[test::NormalizationCount] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'U\"{tests[i][2]}\"')
		file.write('\n\t};\n')
		file.write('}\n')
def CreateEmojiTestFile(outPath: str, inPath: str) -> None:
	print(f'Creating [{outPath}] from [{inPath}] for test [emoji]...')
	tests: list[str] = []

	# open the file which contains the test-sequences and parse it
	with open(inPath, 'r', encoding='utf-8') as file:
		for line in file:
			# remove any comments and skip empty lines
			line = line.split('#')[0].strip()
			if len(line) == 0 or line.startswith('@Part'):
				continue
			if line.endswith(';'):
				line = line[:-1]
			chars = [int(c.strip(), 16) for c in line.split(';')[0].strip().split(' ')]
			chars = ''.join([f'\\U{w:08x}' for w in chars])

			# add the string to the tests (only source, nfc, nfd)
			tests.append(chars)

	# open the file to contain the testing code and write it to the file
	with open(outPath, 'w', encoding='utf-8') as file:
		file.write('#pragma once\n')
		file.write('\n')
		file.write('#include <cinttypes>\n')
		file.write('\n')
		file.write('namespace test {\n')
		file.write(f'\tstatic constexpr size_t EmojiCount = {len(tests)};\n')
		file.write('\n')

		# write all emoji-strings to the file
		file.write(f'\tstatic const char32_t* EmojiStrings[test::EmojiCount] = {{')
		for i in range(len(tests)):
			file.write('\n\t\t' if i == 0 else ',\n\t\t')
			file.write(f'U\"{tests[i]}\"')
		file.write('\n\t};\n')
		file.write('}\n')

# TestUnicode, TestAscii, TestAsciiAlphabetic, TestAsciiNumeric, GetAsciiRadix, TestWhiteSpace, TestControl, GetProperty (encodes: assigned/alphabetic/numeric/decimal/printable/case/category/emoji/bidi)
def MakePropertyLookup(outPath: str, config: SystemConfig) -> None:
	# parse the relevant files
	unicodeData = ParsedFile(config.mapping['UnicodeData'], True)
	derivedProperties = ParsedFile(config.mapping['DerivedCoreProperties'], False)
	propList = ParsedFile(config.mapping['PropList'], False)
	eaWidth = ParsedFile(config.mapping['EastAsianWidth'], False)
	derivedBidi = ParsedFile(config.mapping['DerivedBidiClass'], False)
	emojiData = ParsedFile(config.mapping['EmojiData'], False)

	# write all lookup functions to the file
	with GeneratedFile(outPath, config) as file:
		_type: LookupType = LookupType.intType(0, 'uint32_t')
		propertyDefValue: int = 0

		# write the unicode-test to the file
		unicodeRanges = Ranges.difference([Range(0, 0x10ffff, 1)], unicodeData.values(lambda fs: None if fs[1] != 'Cs' else 1))
		_gen: CodeGen = file.next('Unicode', 'Automatically generated from: Unicode General_Category is not cs (i.e. surrogate pairs) smaller than/equal to 0x10ffff')
		_gen.intFunction('TestUnicode', LookupType.boolType(), unicodeRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the ascii-test to the file
		asciiRanges = [Range(0, 0x7f, 1)]
		_gen: CodeGen = file.next('Ascii', 'Automatically generated from: [cp <= 0x7f]')
		_gen.intFunction('TestAscii', LookupType.boolType(), asciiRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the ascii-alphabetic-test to the file
		asciiAlphabeticRanges = Ranges.fromRawList([Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)])
		_gen: CodeGen = file.next('AsciiAlphabetic', 'Automatically generated from: [a-zA-Z]')
		_gen.intFunction('TestAsciiAlphabetic', LookupType.boolType(), asciiAlphabeticRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the ascii-numeric-test to the file
		asciiNumericRanges = Ranges.fromRawList([Range(ord('0'), ord('9'), 1)])
		_gen: CodeGen = file.next('AsciiNumeric', 'Automatically generated from: [0-9]')
		_gen.intFunction('TestAsciiNumeric', LookupType.boolType(), asciiNumericRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the ascii-radix-getter to the file
		radixRanges = Ranges.fromRawList([Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)])
		radixValueDef = 36
		_gen: CodeGen = file.next('AsciiRadix', f'Automatically generated from: [0-9a-zA-Z] mapped to [0-35] and rest to {radixValueDef}')
		_gen.addConstInt(LookupType.intType(0, 'uint8_t'), 'AsciiRadixNone', radixValueDef)
		_gen.intFunction('GetAsciiRadix', LookupType.intType(radixValueDef, 'uint8_t'), radixRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the whitespace-test to the file (https://www.unicode.org/reports/tr44/#White_Space)
		whiteSpaceRanges = propList.values(lambda fs: None if fs[0] != 'White_Space' else 1)
		_gen: CodeGen = file.next('WhiteSpace', 'Automatically generated from: Unicode White_Space property')
		_gen.intFunction('TestWhiteSpace', LookupType.boolType(), whiteSpaceRanges, CodeGenConfig(CodeGenDensityIfElse(1, 256)))

		# write the control-test to the file (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
		controlRanges = unicodeData.values(lambda fs: None if fs[1] != 'Cc' else 1)
		_gen: CodeGen = file.next('Control', 'Automatically generated from: Unicode General_Category is cc (i.e. C0, C1)')
		_gen.intFunction('TestControl', LookupType.boolType(), controlRanges, CodeGenConfig(CodeGenDensityIfElse()))

		# write the assigned-data to the file (default = False is required as default value of UnicodeData is Cn)
		assignedRanges = unicodeData.values(lambda fs: None if fs[1] in ['Cs', 'Co', 'Cn'] else 1)
		_gen: CodeGen = file.next('Assigned', 'Automatically generated from: Unicode General_Category is not Cn, Cs, Co (i.e. not assigned, surrogate pairs, private use)')
		propertyOffset, propertyBits = 0, 1
		_gen.addConstInt(_type, 'PropertyAssignedOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyAssignedMask', (0x01 << propertyBits) - 1)
		propertyRanges = Ranges.translate(assignedRanges, lambda _, v: (v[0] << propertyOffset))
		propertyDefValue = (False << propertyOffset)

		# write the letter-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic)
		alphabeticRanges = derivedProperties.values(lambda fs: None if fs[0] != 'Alphabetic' else 1)
		_gen: CodeGen = file.next('Alphabetic', 'Automatically generated from: Unicode derived property Alphabetic')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 1
		_gen.addConstInt(_type, 'PropertyAlphabeticOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyAlphabeticMask', (0x01 << propertyBits) - 1)
		propertyRanges = Ranges.merge(Ranges.translate(alphabeticRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (False << propertyOffset) | propertyDefValue

		# write the numeric-test to the file (https://www.unicode.org/reports/tr44/#Numeric_Type)
		numericRanges = unicodeData.values(lambda fs: 1 if fs[7] != '' else None)
		_gen: CodeGen = file.next('Numeric', 'Automatically generated from: Unicode Numeric_Type=Numeric, Numeric_Type=Decimal, Numeric_Type=Digit')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 1
		_gen.addConstInt(_type, 'PropertyNumericOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyNumericMask', (0x01 << propertyBits) - 1)
		propertyRanges = Ranges.merge(Ranges.translate(numericRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (False << propertyOffset) | propertyDefValue

		# write the digit-getter to the file (https://www.unicode.org/reports/tr44/#Numeric_Value)
		decimalDefNone, decimalValSub = 0, 1
		decimalRanges = unicodeData.values(lambda fs: int(fs[5]) + decimalValSub if fs[5] != '' and fs[5] in '0123456789' else None)
		_gen: CodeGen = file.next('Decimal', 'Automatically generated from: Unicode Numeric_Type=Decimal: [0-9]')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 4
		if 9 + decimalValSub >= 2**propertyBits:
			raise RuntimeError('Too few bits to encode all values')
		_gen.addConstInt(_type, 'PropertyDecimalOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyDecimalMask', (0x01 << propertyBits) - 1)
		_gen.addConstInt(_type, 'PropertyDecimalNone', decimalDefNone)
		_gen.addConstInt(_type, 'PropertyDecimalSub', decimalValSub)
		propertyRanges = Ranges.merge(Ranges.translate(decimalRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (decimalDefNone << propertyOffset) | propertyDefValue

		# write the printable-enum to the file (https://en.wikipedia.org/wiki/Graphic_character)
		printableFilterMap = {
			'Lu': 1, 'Ll': 1, 'Lt': 1, 'Lm': 1, 'Lo': 1, 'Mn': 1, 'Mc': 1, 'Me': 1, 'Nd': 1, 'Nl': 1, 'No': 1,
			'Pc': 1, 'Pd': 1, 'Ps': 1, 'Pe': 1, 'Pi': 1, 'Pf': 1, 'Po': 1, 'Sm': 1, 'Sc': 1, 'Sk': 1, 'So': 1,
			'Zs': 2
		}
		printableRanges = unicodeData.values(lambda fs: printableFilterMap[fs[1]] if fs[1] in printableFilterMap else None)
		_enum: LookupType = LookupType.enumType('PrintableType', 'none', ['none', 'printable', 'printSpace'])
		_gen: CodeGen = file.next('Printable', 'Automatically generated from: Unicode General_Category is L*,M*,N*,P*,S* or optionally Zs')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 2
		if len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		_gen.addConstInt(_type, 'PropertyPrintableOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyPrintableMask', (0x01 << propertyBits) - 1)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(Ranges.translate(printableRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset) | propertyDefValue

		# write the cased-enum to the file (https://www.unicode.org/reports/tr44/#Cased)
		caseFilterMap = { 'Lowercase': 1, 'Uppercase': 2 }
		caseRanges = derivedProperties.values(lambda fs: caseFilterMap[fs[0]] if fs[0] in caseFilterMap else None)
		caseRanges = Ranges.union(caseRanges, unicodeData.values(lambda fs: 3 if fs[1] == 'Lt' else None))
		_enum: LookupType = LookupType.enumType('CaseType', 'none', ['none', 'lowerCase', 'upperCase', 'titleCase'])
		_gen: CodeGen = file.next('Case', 'Automatically generated from: Unicode derived property Lowercase, Uppercase or General_Category Lt')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 2
		if len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		_gen.addConstInt(_type, 'PropertyCaseOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyCaseMask', (0x01 << propertyBits) - 1)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(Ranges.translate(caseRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset) | propertyDefValue

		# write the category-enum to the file (default value of UnicodeData is Cn) (https://www.unicode.org/reports/tr44/#GC_Values_Table)
		categoryEnumMap = {
			'Cn': 0, 'Lu': 1, 'Ll': 2, 'Lt': 3, 'Lm': 4, 'Lo': 5, 'Mn': 6, 'Mc': 7, 'Me': 8, 'Nd': 9, 'Nl': 10, 'No': 11,
			'Pc': 12, 'Pd': 13, 'Ps': 14, 'Pe': 15, 'Pi': 16, 'Pf': 17, 'Po': 18, 'Sm': 19, 'Sc': 20, 'Sk': 21, 'So': 22,
			'Zs': 23, 'Zl': 24, 'Zp': 25, 'Cc': 26, 'Cf': 27, 'Cs': 28, 'Co': 29
		}
		categoryRanges = unicodeData.values(lambda fs: categoryEnumMap[fs[1]] if fs[1] in categoryEnumMap else None)
		_enum: LookupType = LookupType.enumType('CategoryType', 'cn', ['cn', 'lu', 'll', 'lt', 'lm', 'lo', 'mn', 'mc', 'me', 'nd', 'nl', 'no', 'pc', 'pd', 'ps', 'pe', 'pi', 'pf', 'po', 'sm', 'sc', 'sk', 'so', 'zs', 'zl', 'zp', 'cc', 'cf', 'cs', 'co'])
		_gen: CodeGen = file.next('Category', 'Automatically generated from: Unicode General_Category')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 5
		if len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		_gen.addConstInt(_type, 'PropertyCategoryOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyCategoryMask', (0x01 << propertyBits) - 1)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(Ranges.translate(categoryRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset) | propertyDefValue

		# write the emoji-state to the file (https://www.unicode.org/reports/tr51)
		emojiType = { 'modBase': 0, 'mod': 1, 'keyCapStart': 2, 'regInd': 3, 'tagSpec': 4, 'textPres': 5, 'emojiPres': 6, 'keyCapEnd': 7, 'zwj': 8, 'tagEnd': 9, '_end': 10 }
		emojiRanges = emojiData.values(lambda fs: emojiType['modBase'] if fs[0] == 'Emoji_Modifier_Base' else None)
		emojiRanges = Ranges.union(emojiRanges, emojiData.values(lambda fs: emojiType['mod'] if fs[0] == 'Emoji_Modifier' else None))
		emojiRanges = Ranges.union(emojiRanges, Ranges.fromRawList([Range(ord(c), ord(c), emojiType['keyCapStart']) for c in '0123456789#*']))
		emojiRanges = Ranges.union(emojiRanges, propList.values(lambda fs: emojiType['regInd'] if fs[0] == 'Regional_Indicator' else None))
		emojiRanges = Ranges.union(emojiRanges, Ranges.fromRawList([Range(c, c, emojiType['tagSpec']) for c in range(0xe0020, 0xe007f)]))
		emojiRanges = Ranges.union(emojiRanges, [Range(0xfe0e, 0xfe0e, emojiType['textPres'])])
		emojiRanges = Ranges.union(emojiRanges, [Range(0xfe0f, 0xfe0f, emojiType['emojiPres'])])
		emojiRanges = Ranges.union(emojiRanges, [Range(0x20e3, 0x20e3, emojiType['keyCapEnd'])])
		emojiRanges = Ranges.union(emojiRanges, [Range(0x200d, 0x200d, emojiType['zwj'])])
		emojiRanges = Ranges.union(emojiRanges, [Range(0xe007f, 0xe007f, emojiType['tagEnd'])])
		_enum: LookupType = LookupType.enumType('EmojiType', 'modBase', emojiType)
		_gen: CodeGen = file.next('Emoji', 'Automatically generated from: Unicode Emoji/Emoji_Modifier_Base/Emoji_Modifier/Emoji_Presentation/...')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 6
		if 2 + len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		flagIsEmoji = 0x01 << propertyOffset
		flagIsEmojiPresentation = 0x02 << propertyOffset
		_gen.addConstInt(_type, 'PropertyIsEmoji', flagIsEmoji)
		_gen.addConstInt(_type, 'PropertyIsPresentation', flagIsEmojiPresentation)
		_gen.addConstInt(_type, 'PropertyEmojiOff', propertyOffset + 2)
		_gen.addConstInt(_type, 'PropertyEmojiMask', (0x01 << (propertyBits - 2)) - 1)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(emojiData.values(lambda fs: flagIsEmoji if fs[0] == 'Emoji' else None), propertyRanges, lambda a, b: a[0]|b[0])
		propertyRanges = Ranges.merge(emojiData.values(lambda fs: flagIsEmojiPresentation if fs[0] == 'Emoji_Presentation' else None), propertyRanges, lambda a, b: a[0]|b[0])
		propertyRanges = Ranges.merge(Ranges.translate(emojiRanges, lambda _, v: (v[0] << propertyOffset + 2)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset + 2) | propertyDefValue

		# write the east-asian-width data to the file (https://www.unicode.org/reports/tr11)
		eaIdMap = { 'N': 0, 'F': 1, 'H': 2, 'W': 3, 'Na': 4, 'A': 5 }
		eaRanges, eaRangesDef = eaWidth.singleMissing(lambda fs: eaIdMap[fs[0]])
		if eaRangesDef != (eaIdMap['N'],):
			raise RuntimeError('East-Asian-Width default value is expected to be neutral')
		_enum: LookupType = LookupType.enumType('EAWidthType', 'neutral', ['neutral', 'fullWidth', 'halfWidth', 'wide', 'narrow', 'ambiguous'])
		_gen: CodeGen = file.next('EastAsianWidth', 'Automatically generated from: Unicode East_Asian_Width')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 3
		if len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		_gen.addConstInt(_type, 'PropertyEAWidthOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyEAWidthMask', (0x01 << propertyBits) - 1)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(Ranges.translate(eaRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset) | propertyDefValue

		# write the bidi-class property to the file (https://www.unicode.org/reports/tr9)
		bidiLargeToShort = { 'Left_To_Right': 'L', 'Right_To_Left': 'R', 'Arabic_Letter': 'AL', 'European_Number': 'EN', 'European_Separator': 'ES', 'European_Terminator': 'ET', 'Arabic_Number': 'AN', 'Common_Separator': 'CS',
							'Nonspacing_Mark': 'NSM', 'Boundary_Neutral': 'BN', 'Paragraph_Separator': 'B', 'Segment_Separator': 'S', 'White_Space': 'WS', 'Other_Neutral': 'ON', 'Left_To_Right_Embedding': 'LRE', 'Left_To_Right_Override': 'LRO',
							'Right_To_Left_Embedding': 'RLE', 'Right_To_Left_Override': 'RLO', 'Pop_Directional_Format': 'PDF', 'Left_To_Right_Isolate': 'LRI', 'Right_To_Left_Isolate': 'RLI', 'First_Strong_Isolate': 'FSI', 'Pop_Directional_Isolate': 'PDI' }
		bidiMap = { 'L': 0, 'R': 1, 'AL': 2, 'EN': 3, 'ES': 4, 'ET': 5, 'AN': 6, 'CS': 7, 'NSM': 8, 'BN': 9, 'B': 10, 'S': 11, 'WS': 12,
			'ON': 13, 'LRE': 14, 'LRO': 15, 'RLE': 16, 'RLO': 17, 'PDF': 18, 'LRI': 19, 'RLI': 20, 'FSI': 21, 'PDI': 22 }
		bidiMap |= { alt: bidiMap[bidiLargeToShort[alt]] for alt in bidiLargeToShort }
		bidiRanges, bidiRangesDef = derivedBidi.multiMissing(lambda fs: bidiMap[fs[0]])
		if bidiRangesDef != (bidiMap['Left_To_Right'],):
			raise RuntimeError('BiDi default value is expected to be Left_To_Right')
		_enum: LookupType = LookupType.enumType('BidiType', 'l', ['l', 'r', 'al', 'en', 'es', 'et', 'an', 'cs', 'nsm', 'bn', 'b', 's', 'ws', 'on', 'lre', 'lro', 'rle', 'rlo', 'pdf', 'lri', 'rli', 'fsi', 'pdi', '_end'])
		_gen: CodeGen = file.next('BidiClass', 'Automatically generated from: Bidi_Class (Currently unused)')
		propertyOffset, propertyBits = (propertyOffset + propertyBits), 5
		if len(_enum.enumValues()) > 2**propertyBits:
			raise RuntimeError('Too few bits to encode all enum values')
		_gen.addConstInt(_type, 'PropertyBidiOff', propertyOffset)
		_gen.addConstInt(_type, 'PropertyBidiMask', (0x01 << propertyBits) - 1)
		_gen.addConstInt(_type, 'BidiMaxDepth', 125)
		_gen.addEnum(_enum)
		propertyRanges = Ranges.merge(Ranges.translate(bidiRanges, lambda _, v: (v[0] << propertyOffset)), propertyRanges, lambda a, b: a[0]|b[0])
		propertyDefValue = (_enum.defValue() << propertyOffset) | propertyDefValue

		# add the final property-lookup function for all large-lookup values (requires less memory than separate lookups)
		if propertyDefValue != 0:
			raise RuntimeError('Default property value should be null or ensured to be initialized properly for all other ranges merged into it (might otherwise be null-initialized)')
		_gen: CodeGen = file.next('Property', 'Lookup properties (BidiClass, Emoji, Category, Case, Printable, Decimal, Numeric, Alphabetic, Assigned)')
		_gen.intFunction('GetProperty', LookupType.intType(propertyDefValue, _type.typeName()), propertyRanges, CodeGenConfig(CodeGenIndirect(), CodeGenDensityIfElse()))

# MapCase (encodes: lowercase/uppercase/titlecase/case-folding mappings)
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
def _TranslateSpecialCaseMap(cIndex: int, values: tuple[int], nullCondition: int, negativeFlag: int, valueMask: int) -> tuple[int]:
	index, out, nulls = 0, [], []

	# skip all conditions and only offset all actual values by the character and separate the null-conditions out to append them in the end
	while index < len(values):
		# write the condition and value-count out
		to = (nulls if (values[index] & valueMask) == nullCondition else out)
		to.append(values[index])
		to.append(values[index + 1])

		# validate all separate values of the payload and write them out
		for i in range(index + 2, index + 2 + values[index + 1]):
			val = values[i] - cIndex
			if abs(val) > valueMask:
				raise RuntimeError('Too few bits to encode value')
			to.append(abs(val) | (negativeFlag if val < 0 else 0))
		index += 2 + values[index + 1]
	return tuple(out + nulls)
def _TranslateSimpleCaseValue(tpFlag: int, negativeFlag: int, valueMask: int, value: int) -> tuple[int]|None:
	if value == 0:
		return None

	# check if the value can fit into a single cell
	if abs(value) > valueMask:
		raise RuntimeError('Too few bits to encode simple casing values')
	return (abs(value) | tpFlag | (0 if value >= 0 else negativeFlag),)
def _TranslateCleanupCaseMap(values: tuple[int], foldFlag: int, lowerFlag: int, upperFlag: int, titleFlag: int, nullCondition: int, valueMask: int) -> tuple[int]:
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
	foldDef, lowerDef, upperDef, titleDef = [0], [0], [0], [0]
	for i in range(len(indices) - 1, -1, -1):
		iValues = modified[indices[i] + 2:indices[i] + 2 + modified[indices[i] + 1]]

		# iterate over the flags and check if they can be removed from the current index
		for f, d in [(foldFlag, foldDef), (lowerFlag, lowerDef), (upperFlag, upperDef), (titleFlag, titleDef)]:
			if (modified[indices[i]] & f) == 0:
				continue
			if iValues == d:
				modified[indices[i]] ^= f

		# update the default values for all null-conditions and clear the current condition-bits for
		# all upcoming values, as this is a null-condition and will therefore be true at all times
		if (modified[indices[i]] & valueMask) == nullCondition:
			for j in range(i + 1, len(indices)):
				modified[indices[j]] &= ~(modified[indices[i]] & (foldFlag | lowerFlag | upperFlag | titleFlag))
			if (modified[indices[i]] & foldFlag) != 0:
				foldDef = iValues
			if (modified[indices[i]] & lowerFlag) != 0:
				lowerDef = iValues
			if (modified[indices[i]] & upperFlag) != 0:
				upperDef = iValues
			if (modified[indices[i]] & titleFlag) != 0:
				titleDef = iValues

		# null the default-values as they will not be reached anymore
		else:
			if (modified[indices[i]] & foldFlag) != 0:
				foldDef = None
			if (modified[indices[i]] & lowerFlag) != 0:
				lowerDef = None
			if (modified[indices[i]] & upperFlag) != 0:
				upperDef = None
			if (modified[indices[i]] & titleFlag) != 0:
				titleDef = None

	# remove all indices, which have empty casing-flags, and merge neighboring similar types
	out, last, typeFlags = [], None, (foldFlag | lowerFlag | upperFlag | titleFlag)
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
def _MergeCaseMap(a: tuple[int], b: tuple[int], nullCondition: int, typeFlags: int, negativeFlag: int, valueMask: int) -> tuple[int]:
	# check if the values are the same and only differ by the type-flats
	if len(a) == 1 and len(b) == 1 and ((a[0] ^ b[0]) & ~typeFlags) == 0:
		return (a[0] | b[0],)

	# check if any of the values is a single cell, in which case it must be expanded to the null-condition
	if len(a) == 1:
		a = (nullCondition | (a[0] & ~(valueMask | negativeFlag)), 1, a[0] & (valueMask | negativeFlag))
	if len(b) == 1:
		b = (nullCondition | (b[0] & ~(valueMask | negativeFlag)), 1, b[0] & (valueMask | negativeFlag))
	return a + b
def _ExpandCaseFolding(tTypes: bool, typeVal: str, values: str) -> tuple[int]|None:
	if typeVal == 'S' or (typeVal != 'T' if tTypes else typeVal == 'T'):
		return None
	if typeVal != 'C' and typeVal != 'F' and typeVal != 'T':
		raise RuntimeError('Unsupported type for case-folding encountered')
	return tuple(int(u, 16) for u in values.split(' '))
def _TranslateFolding(cIndex: int, values: tuple[int], flagIsFold: int, condition: int, flagIsNegative: int, valueMask: int, condIsNull: bool) -> tuple[int]:
	out = []

	# validate all values are small enough and translate them
	for v in values:
		v = v - cIndex
		if abs(v) > valueMask:
			raise RuntimeError('Too few bits to encode folding value')
		out.append(abs(v) | (flagIsNegative if v < 0 else 0))

	# check if an initial condition needs to be added and add the flag
	if len(out) > 1 or not condIsNull:
		out = [condition, len(out)] + out
	out[0] = out[0] | flagIsFold
	return tuple(out)
def MakeCasingLookup(outPath: str, config: SystemConfig) -> None:
	# parse the relevant files
	unicodeData = ParsedFile(config.mapping['UnicodeData'], True)
	specialCasing = ParsedFile(config.mapping['SpecialCasing'], False)
	derivedProperties = ParsedFile(config.mapping['DerivedCoreProperties'], False)
	propList = ParsedFile(config.mapping['PropList'], False)
	caseFolding = ParsedFile(config.mapping['CaseFolding'], False)

	# write all map functions to the file
	with GeneratedFile(outPath, config) as file:
		# encoding:
		#	uint20_t valueOrCondition
		#	uint1_t valueIsFoldingMapping
		#	uint1_t valueIsTitleMapping
		#	uint1_t valueIsUpperMapping
		#	uint1_t valueIsLowerMapping
		#	uint1_t valueIsNegative
		#	uint1_t is \u0307 - value (Before_Dot; OnlyInFirstValue)
		#	uint1_t is \u0049 - value (After_I; OnlyInFirstValue)
		#	uint1_t is ccc=230 (More_Above; OnlyInFirstValue)
		#	uint1_t is ccc=0|ccc=230 (After_Soft_Dotted; More_Above; Before_Dot; After_I; OnlyInFirstValue)
		#	uint1_t is soft-dotted type (After_Soft_Dotted; OnlyInFirstValue)
		#	uint1_t is case-ignorable (Final_Sigma; OnlyInFirstValue)
		#	uint1_t is cased (Final_Sigma; OnlyInFirstValue)
		#
		#	all values are relative to current codepoint to support further reduction/merging of the ranges
		#	If length of chain is 1: value is the direct mapping, and value-negative flag applies (for all, which match the is...Mapping flag)
		#	Otherwise, it consists of chain of condition-values, followed by single size-byte and the given size-number of values.
		#		for each value, valueIsNegative is used
		#		for each condition: is...Mapping is used

		# define the flags used for the separate values (keep the topmost bit clear as value is signed)
		flagIsCased = 0x8000_0000
		flagIsIgnorable = 0x4000_0000
		flagIsSoftDotted = 0x2000_0000
		flagCombClass0or230 = 0x1000_0000
		flagCombClass230 = 0x0800_0000
		flagIs0049 = 0x0400_0000
		flagIs0307 = 0x0200_0000
		flagIsNegative = 0x0100_0000
		flagIsLower = 0x0080_0000
		flagIsUpper = 0x0040_0000
		flagIsTitle = 0x0020_0000
		flagIsFold = 0x0010_0000
		valueMask = 0x000f_ffff

		# extract all attributes and merge them together
		propertyMask = derivedProperties.values(lambda fs: flagIsCased if fs[0] == 'Cased' else None)
		propertyMask = Ranges.merge(propertyMask, derivedProperties.values(lambda fs: flagIsIgnorable if fs[0] == 'Case_Ignorable' else None), lambda a, b: (a[0] | b[0],))
		propertyMask = Ranges.merge(propertyMask, propList.values(lambda fs: flagIsSoftDotted if fs[0] == 'Soft_Dotted' else None), lambda a, b: (a[0] | b[0],))
		propertyMask = Ranges.merge(propertyMask, unicodeData.values(lambda fs: flagCombClass0or230 if (fs[2] == '0' or fs[2] == '230') else None), lambda a, b: (a[0] | b[0],))
		propertyMask = Ranges.merge(propertyMask, unicodeData.values(lambda fs: flagCombClass230 if fs[2] == '230' else None), lambda a, b: (a[0] | b[0],))
		propertyMask = Ranges.merge(propertyMask, [Range(0x0049, 0x0049, flagIs0049), Range(0x0307, 0x0307, flagIs0307)], lambda a, b: (a[0] | b[0],))

		# extract all special casing rules (as 'sorted' is stable, values are guaranteed to be ordered by appearance)
		caseConditions = { 'none': 0, 'trOrAz': 1 }
		specialRanges = specialCasing.conflicting(lambda fs: _ExpandSpecialCaseMap(caseConditions, flagIsLower, flagIsUpper, flagIsTitle, fs[0], fs[1], fs[2], fs[3]), lambda a, b: _MergeCaseMap(a, b, 0, 0, 0, 0))

		# sanitize and cleanup the special ranges, as all none-conditions must lie in the end, as conditions are more relevant, if they match
		specialRanges = Ranges.translate(specialRanges, lambda c, v: _TranslateSpecialCaseMap(c, v, caseConditions['none'], flagIsNegative, valueMask))
		caseConditions['_end'] = len(caseConditions)
		if len(caseConditions) > valueMask:
			raise RuntimeError('Conditions overflow values')

		# extract all simple lower-case mappings
		simpleLowerRanges = unicodeData.values(lambda fs: int(fs[12], 16) if fs[12] != '' else None)
		simpleLowerRanges = Ranges.translate(simpleLowerRanges, lambda c, v: _TranslateSimpleCaseValue(flagIsLower, flagIsNegative, valueMask, v[0] - c))

		# extract all simple upper-case mappings
		simpleUpperRanges = unicodeData.values(lambda fs: int(fs[11], 16) if fs[11] != '' else None)
		simpleUpperRanges = Ranges.translate(simpleUpperRanges, lambda c, v: _TranslateSimpleCaseValue(flagIsUpper, flagIsNegative, valueMask, v[0] - c))

		# extract all simple title-case mappings
		simpleTitleRanges = unicodeData.values(lambda fs: int(fs[13], 16) if fs[13] != '' else None)
		simpleTitleRanges = Ranges.translate(simpleTitleRanges, lambda c, v: _TranslateSimpleCaseValue(flagIsTitle, flagIsNegative, valueMask, v[0] - c))

		# extract all fold-case mappings and merge them the complex ranges
		simpleFoldRanges = caseFolding.values(lambda fs: _ExpandCaseFolding(False, fs[0], fs[1]))
		simpleFoldRanges = Ranges.translate(simpleFoldRanges, lambda c, v: _TranslateFolding(c, v, flagIsFold, caseConditions['none'], flagIsNegative, valueMask, True))
		foldingRanges = caseFolding.values(lambda fs: _ExpandCaseFolding(True, fs[0], fs[1]))
		foldingRanges = Ranges.translate(foldingRanges, lambda c, v: _TranslateFolding(c, v, flagIsFold, caseConditions['trOrAz'], flagIsNegative, valueMask, False))
		specialRanges = Ranges.merge(foldingRanges, specialRanges, lambda a, b: a + b)

		# merge the simple values together (lower, upper, fold, title)
		simpleRanges = Ranges.merge(simpleLowerRanges, simpleUpperRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsFold | flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, valueMask))
		simpleRanges = Ranges.merge(simpleRanges, simpleFoldRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsFold | flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, valueMask))
		simpleRanges = Ranges.merge(simpleRanges, simpleTitleRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsFold | flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, valueMask))

		# merge the special ranges and simple ranges together (special-ranges before simple-ranges to preserve order in
		# reconstructed final ranges) and clean the ranges up to remove irrelevant conditions and combine the properties into it
		caseRanges = Ranges.merge(specialRanges, simpleRanges, lambda a, b: _MergeCaseMap(a, b, caseConditions['none'], flagIsFold | flagIsLower | flagIsUpper | flagIsTitle, flagIsNegative, valueMask))
		caseRanges = Ranges.translate(caseRanges, lambda _, v: _TranslateCleanupCaseMap(v, flagIsFold, flagIsLower, flagIsUpper, flagIsTitle, caseConditions['none'], valueMask))
		caseRanges = Ranges.merge(caseRanges, propertyMask, lambda a, b: (a[0] | b[0],) + a[1:])

		# update the casing of the condition map
		tempConditions, caseConditions = caseConditions, {}
		for k in tempConditions:
			n, i = '', 0
			if k == '_end':
				n = k
			else:
				while i < len(k):
					if k[i] != '_':
						n += k[i]
					else:
						i += 1
						n += k[i].upper()
					i += 1
			caseConditions[n] = tempConditions[k]

		# write the case-mapping and enum to the file (https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf, https://www.unicode.org/reports/tr44/#Casemapping)
		_type: LookupType = LookupType.intType(0, 'uint32_t')
		_gen: CodeGen = file.next('Case', 'Automatically generated from: Special-Casing, unicode-data simple case mapping')
		_gen.addConstInt(_type, 'CaseIsNegative', flagIsNegative)
		_gen.addConstInt(_type, 'CaseIsCased', flagIsCased)
		_gen.addConstInt(_type, 'CaseIsIgnorable', flagIsIgnorable)
		_gen.addConstInt(_type, 'CaseIsSoftDotted', flagIsSoftDotted)
		_gen.addConstInt(_type, 'CaseIsCombClass0or230', flagCombClass0or230)
		_gen.addConstInt(_type, 'CaseIsCombClass230', flagCombClass230)
		_gen.addConstInt(_type, 'CaseIs0049', flagIs0049)
		_gen.addConstInt(_type, 'CaseIs0307', flagIs0307)
		_gen.addConstInt(_type, 'CaseIsLower', flagIsLower)
		_gen.addConstInt(_type, 'CaseIsUpper', flagIsUpper)
		_gen.addConstInt(_type, 'CaseIsTitle', flagIsTitle)
		_gen.addConstInt(_type, 'CaseIsFold', flagIsFold)
		_gen.addConstInt(_type, 'CaseValueMask', valueMask)
		_gen.addEnum(LookupType.enumType('CaseCond', 'none', caseConditions))
		_gen.listFunction('MapCase', _type, caseRanges, False, CodeGenConfig(CodeGenIndirect(), CodeGenDensityIfElse(1/4)))

# GetSegmentation (encodes: word/grapheme/sentence/line segmentation)
def _SegmentationMergeConflicts(a: tuple[int], b: tuple[int], conflictMap: dict[tuple[int, int], int], desc: str) -> tuple[int]:
	if (a[0], b[0]) not in conflictMap:
		raise RuntimeError(f'Unexpected conflict encountered while merging {desc}')
	return (conflictMap[(a[0], b[0])],)
def _CnPictographicMerge(a: tuple[int], idCnPictographic: int, id: int) -> tuple[int]:
	if a[0] != id:
		raise RuntimeError('Cn&Extended_Pictographic is expected to at most collide with [ID]')
	return (idCnPictographic,)
def MakeSegmentationLookup(outPath: str, config: SystemConfig) -> None:
	# parse the relevant files
	wordBreak = ParsedFile(config.mapping['WordBreakProperty'], False)
	graphemeBreak = ParsedFile(config.mapping['GraphemeBreakProperty'], False)
	sentenceBreak = ParsedFile(config.mapping['SentenceBreakProperty'], False)
	lineBreak = ParsedFile(config.mapping['LineBreak'], False)
	emojiData = ParsedFile(config.mapping['EmojiData'], False)
	eastAsianWidth = ParsedFile(config.mapping['EastAsianWidth'], False)
	derivedProperties = ParsedFile(config.mapping['DerivedCoreProperties'], False)
	unicodeData = ParsedFile(config.mapping['UnicodeData'], True)

	# write all maps functions to the file
	with GeneratedFile(outPath, config) as file:
		segmentationRanges, segmentationDefValue, segmentationBits, segmentationOffset = [], 0, 6, 0
		_type8: LookupType = LookupType.intType(0, 'uint8_t')

		# setup the word-break boundary ranges (https://unicode.org/reports/tr29/#Word_Boundaries)
		wordEnumMap = { 'Other': 0, 'CR': 1, 'LF': 2, 'Newline': 3, 'Extend': 4, 'ZWJ': 5, 'Regional_Indicator': 6, 'Format': 7,
			'Katakana': 8, 'Hebrew_Letter': 9, 'ALetter': 10, 'Single_Quote': 11, 'Double_Quote': 12, 'MidNumLet': 13, 'MidLetter': 14,
			'MidNum': 15, 'Numeric': 16, 'ExtendNumLet': 17, 'WSegSpace': 18 }
		wordRanges, wordRangesDef = wordBreak.singleMissing(lambda fs: wordEnumMap[fs[0]])
		if wordRangesDef != (wordEnumMap['Other'],):
			raise RuntimeError('Default break-value is expected to be [other]')
		wordEnumMap |= { 'Extended_Pictographic': 19, 'ALetterExtendedPictographic': 20 }
		wordConflictMap = { (wordEnumMap['ALetter'], wordEnumMap['Extended_Pictographic']): wordEnumMap['ALetterExtendedPictographic'] }
		wordRanges = Ranges.merge(wordRanges, emojiData.values(lambda fs: wordEnumMap[fs[0]] if fs[0] == 'Extended_Pictographic' else None), lambda a, b: _SegmentationMergeConflicts(a, b, wordConflictMap, 'word ranges and emoji properties'))

		# write the word-ranges to the file
		_enum: LookupType = LookupType.enumType('WordType', 'other', ['other', 'cr', 'lf', 'newline', 'extend', 'zwj', 'regionalIndicator', 'format', 'katakana', 'hebrewLetter', 'aLetterDef', 'singleQuote', 'doubleQuote', 'midNumLetter', 'midLetter', 'midNum', 'numeric', 'extendNumLet', 'wSegSpace', 'extendedPictographic', 'aLetterExtendedPictographic', '_end'])
		_gen: CodeGen = file.next('Word', 'Automatically generated from: Unicode WordBreakProperty and EmojiData')
		_gen.addConstInt(_type8, 'WordSegmentationOff', segmentationOffset)
		_gen.addEnum(_enum)
		segmentationRanges = Ranges.merge(segmentationRanges, Ranges.translate(wordRanges, lambda _, v: v[0] << segmentationOffset), lambda a, b: (a[0] | b[0],))
		segmentationDefValue = (wordRangesDef[0] << segmentationOffset) | segmentationDefValue
		if len(_enum.enumValues()) > (1 << segmentationBits):
			raise RuntimeError(f'Enum {_enum.typeName()} does not fit into {segmentationBits} bits')
		segmentationOffset += segmentationBits

		# setup the grapheme-break boundary ranges (https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries)
		graphemeEnumMap = {
			'Other': 0, 'CR': 1, 'LF': 2, 'Control': 3, 'Extend': 4, 'ZWJ': 5,
			'Regional_Indicator': 6, 'Prepend': 7, 'SpacingMark': 8, 'L': 9, 'V': 10,
			'T': 11, 'LV': 12, 'LVT': 13, 'Extended_Pictographic': 14 }
		graphemeRanges, graphemeRangesDef = graphemeBreak.singleMissing(lambda fs: graphemeEnumMap[fs[0]])
		if graphemeRangesDef != (graphemeEnumMap['Other'],):
			raise RuntimeError('Default break-value is expected to be [other]')
		graphemeRanges = Ranges.union(graphemeRanges, emojiData.values(lambda fs: graphemeEnumMap[fs[0]] if fs[0] == 'Extended_Pictographic' else None))
		graphemeEnumMap |= { 'InCBExtend': 15, 'InCBConsonant': 16, 'InCBLinker': 17, 'ExtendInCBExtend': 18, 'ExtendInCBLinker': 19, 'ZWJInCBExtend': 20 }
		inCBMap = { 'Extend': graphemeEnumMap['InCBExtend'], 'Consonant': graphemeEnumMap['InCBConsonant'], 'Linker': graphemeEnumMap['InCBLinker'] }
		inCBConflictMap = {
			(graphemeEnumMap['Extend'], graphemeEnumMap['InCBExtend']): graphemeEnumMap['ExtendInCBExtend'],
			(graphemeEnumMap['Extend'], graphemeEnumMap['InCBLinker']): graphemeEnumMap['ExtendInCBLinker'],
			(graphemeEnumMap['ZWJ'], graphemeEnumMap['InCBExtend']): graphemeEnumMap['ZWJInCBExtend'] }
		graphemeRanges = Ranges.merge(graphemeRanges, derivedProperties.values(lambda fs: inCBMap[fs[1]] if fs[0] == 'InCB' else None, True), lambda a, b: _SegmentationMergeConflicts(a, b, inCBConflictMap, 'grapheme ranges and InCB properties'))

		# write the grapheme-ranges to the file
		_enum: LookupType = LookupType.enumType('GraphemeType', 'other', ['other', 'cr', 'lf', 'control', 'extendDef', 'zwjDef', 'regionalIndicator', 'prepend', 'spaceMarking', 'l', 'v', 't', 'lv', 'lvt', 'extendedPictographic', 'inCBExtend', 'inCBConsonant', 'inCBLinker', 'extendInCBExtend', 'extendInCBLinker', 'zwjInCBExtend', '_end'])
		_gen: CodeGen = file.next('Grapheme', 'Automatically generated from: Unicode GraphemeBreakProperty, EmojiData, and DerivedProperties')
		_gen.addConstInt(_type8, 'GraphemeSegmentationOff', segmentationOffset)
		_gen.addEnum(_enum)
		segmentationRanges = Ranges.merge(segmentationRanges, Ranges.translate(graphemeRanges, lambda _, v: v[0] << segmentationOffset), lambda a, b: (a[0] | b[0],))
		segmentationDefValue = (graphemeRangesDef[0] << segmentationOffset) | segmentationDefValue
		if len(_enum.enumValues()) > (1 << segmentationBits):
			raise RuntimeError(f'Enum {_enum.typeName()} does not fit into {segmentationBits} bits')
		segmentationOffset += segmentationBits

		# setup the sentence-break boundary ranges (https://unicode.org/reports/tr29/#Sentence_Boundaries)
		sentenceEnumMap = {
			'Other': 0, 'CR': 1, 'LF': 2, 'Extend': 3, 'Sep': 4, 'Format': 5, 'Sp': 6, 'Lower': 7, 'Upper': 8,
			'OLetter': 9, 'Numeric': 10, 'ATerm': 11, 'SContinue': 12, 'STerm': 13, 'Close': 14 }
		sentenceRanges, sentenceRangesDef = sentenceBreak.singleMissing(lambda fs: sentenceEnumMap[fs[0]])
		if sentenceRangesDef != (sentenceEnumMap['Other'],):
			raise RuntimeError('Default break-value is expected to be [other]')

		# write the sentence-ranges to the file
		_enum: LookupType = LookupType.enumType('SentenceType', 'other', ['other', 'cr', 'lf', 'extend', 'separator', 'format', 'space', 'lower', 'upper', 'oLetter', 'numeric', 'aTerm', 'sContinue', 'sTerm', 'close', '_end'])
		_gen: CodeGen = file.next('Sentence', 'Automatically generated from: Unicode SentenceBreakProperty')
		_gen.addConstInt(_type8, 'SentenceSegmentationOff', segmentationOffset)
		_gen.addEnum(_enum)
		segmentationRanges = Ranges.merge(segmentationRanges, Ranges.translate(sentenceRanges, lambda _, v: v[0] << segmentationOffset), lambda a, b: (a[0] | b[0],))
		segmentationDefValue = (sentenceRangesDef[0] << segmentationOffset) | segmentationDefValue
		if len(_enum.enumValues()) > (1 << segmentationBits):
			raise RuntimeError(f'Enum {_enum.typeName()} does not fit into {segmentationBits} bits')
		segmentationOffset += segmentationBits

		# setup the line-break boundary ranges and list of the enum (https://www.unicode.org/reports/tr14/#Algorithm)
		lineEnumMap = {
			'BK':  0, 'CR':  1, 'LF':  2, 'CM':  3, 'NL':  4, 'WJ':  5, 'ZW':  6, 'GL':  7, 'SP':  8, 'ZWJ': 9, 'B2': 10,
			'BA': 11, 'BB': 12, 'HY': 13, 'CB': 14, 'CL': 15, 'CP': 16, 'EX': 17, 'IN': 18, 'NS': 19, 'OP': 20, 'QU': 21,
			'IS': 22, 'NU': 23, 'PO': 24, 'PR': 25, 'SY': 26, 'AK': 27, 'AL': 28, 'AP': 29, 'AS': 30, 'EB': 31, 'EM': 32,
			'H2': 33, 'H3': 34, 'HL': 35, 'ID': 36, 'JL': 37, 'JV': 38, 'JT': 39, 'RI': 40, 'VF': 41, 'VI': 42,
			'XX': 43, 'CJ': 44, 'AI': 45, 'SG': 46, 'SA': 47 }
		lineRanges, lineRangesDef = lineBreak.singleMissing(lambda fs: lineEnumMap[fs[0]])
		if lineRangesDef != (lineEnumMap['XX'],):
			raise RuntimeError('Default break-value is expected to be [XX]')
		lineEnumList = [0] * len(lineEnumMap)
		for k in lineEnumMap:
			lineEnumList[lineEnumMap[k]] = k.lower()
		lineEnumList[lineEnumMap['AL']] = 'alDef'
		lineEnumList[lineEnumMap['BA']] = 'baDef'
		lineEnumList[lineEnumMap['QU']] = 'quNoPiPf'
		lineEnumList[lineEnumMap['ID']] = 'idDef'

		# setup the intermediate helper-ranges
		categoryMnOrMc = unicodeData.values(lambda fs: 1 if (fs[1] == 'Mn' or fs[1] == 'Mc') else None)
		fwhAsianRanges = eastAsianWidth.values(lambda fs: 1 if fs[0] in 'FWH' else None)
		cnPictRanges = Ranges.intersect(Ranges.complement(unicodeData.values(lambda fs: 1 if fs[1] != 'Cn' else None)), emojiData.values(lambda fs: 1 if fs[0] == 'Extended_Pictographic' else None))

		# LB1 mapping
		lineRanges = Ranges.translate(lineRanges, lambda _, v: lineEnumMap['AL'] if v[0] == lineEnumMap['AI'] else v)
		lineRanges = Ranges.translate(lineRanges, lambda _, v: lineEnumMap['AL'] if v[0] == lineEnumMap['SG'] else v)
		lineRanges = Ranges.translate(lineRanges, lambda _, v: lineEnumMap['AL'] if v[0] == lineEnumMap['XX'] else v)
		lineRangesDef = lineEnumMap['AL']
		lineRanges = Ranges.translate(lineRanges, lambda _, v: lineEnumMap['NS'] if v[0] == lineEnumMap['CJ'] else v)
		lineRanges = Ranges.modify(lineRanges, categoryMnOrMc, lambda a, _: lineEnumMap['CM'] if a[0] == lineEnumMap['SA'] else a)
		lineRanges = Ranges.modify(lineRanges, Ranges.complement(categoryMnOrMc), lambda a, _: lineEnumMap['AL'] if a[0] == lineEnumMap['SA'] else a)
		lineEnumList = lineEnumList[:-5]

		# add the special values
		if Ranges.lookup(lineRanges, 0x25cc) != (lineEnumMap['AL'],):
			raise RuntimeError('Dotted-Circle is assumed to be part of the [AL] break-type')
		lineRanges = Ranges.merge(lineRanges, [Range(0x25cc, 0x25cc, 1)], lambda a, b: len(lineEnumList))
		lineEnumList.append('alDotCircle')
		if Ranges.lookup(lineRanges, 0x2010) != (lineEnumMap['BA'],):
			raise RuntimeError('Hypen is assumed to be part of the [BA] break-type')
		lineRanges = Ranges.merge(lineRanges, [Range(0x2010, 0x2010, 1)], lambda a, b: len(lineEnumList))
		lineEnumList.append('baHyphen')
		lineRanges = Ranges.modify(lineRanges, unicodeData.values(lambda fs: 1 if fs[1] == 'Pi' else None), lambda a, _: (a if a[0] != lineEnumMap['QU'] else len(lineEnumList)))
		lineEnumList.append('quPi')
		lineRanges = Ranges.modify(lineRanges, unicodeData.values(lambda fs: 1 if fs[1] == 'Pf' else None), lambda a, _: (a if a[0] != lineEnumMap['QU'] else len(lineEnumList)))
		lineEnumList.append('quPf')
		lineRanges = Ranges.merge(lineRanges, Ranges.translate(cnPictRanges, lambda c, v: len(lineEnumList)), lambda a, _: _CnPictographicMerge(a, len(lineEnumList) + 1, lineEnumMap['ID']))
		lineEnumList.append('defCnPict')
		lineEnumList.append('idCnPict')
		lineEnumList.append('_end')

		# write the line-ranges to the file
		_enum: LookupType = LookupType.enumType('LineType', lineEnumList[lineRangesDef], lineEnumList)
		_gen: CodeGen = file.next('Line', 'Automatically generated from: Unicode LineBreak, General_Category, East-Asian-Width, EmojiData')
		_gen.addConstInt(_type8, 'LineSegmentationOff', segmentationOffset)
		_gen.addEnum(_enum)
		segmentationRanges = Ranges.merge(segmentationRanges, Ranges.translate(lineRanges, lambda _, v: v[0] << segmentationOffset), lambda a, b: (a[0] | b[0],))
		segmentationDefValue = (lineRangesDef << segmentationOffset) | segmentationDefValue
		if len(_enum.enumValues()) > (1 << segmentationBits):
			raise RuntimeError(f'Enum {_enum.typeName()} does not fit into {segmentationBits} bits')
		segmentationOffset += segmentationBits

		# write the east asian width out
		_gen.addConstInt(_type8, 'LineFWHAsianWidthTest', segmentationOffset)
		segmentationRanges = Ranges.merge(segmentationRanges, Ranges.translate(fwhAsianRanges, lambda c, v: (1 << segmentationOffset)), lambda a, b: (a[0] | b[0],))
		segmentationOffset += 1

		# create the merged function to encode all four segmentation-lookups into a single buffer-function (requires less memory than four separate lookups)
		if segmentationOffset > 32:
			raise RuntimeError(f'Segmentation does not fit into {32} bits (is {segmentationOffset})')
		_type32: LookupType = LookupType.intType(segmentationDefValue, 'uint32_t')
		_gen: CodeGen = file.next('Segmentation', 'Lookup segmentation types (Word, Grapheme, Sentence, Line, EastAsianWidthFWH)')
		_gen.addConstInt(_type8, 'SegmentationMask', (1 << segmentationBits) - 1)
		_gen.intFunction('GetSegmentation', _type32, segmentationRanges, CodeGenConfig(CodeGenIndirect(), CodeGenDensityIfElse()))

# GetNormalization (encodes ccc/decomposition/composition states)
def _ParseCCC(val: str) -> int|None:
	if val == '0':
		return None
	out = int(val)
	if out > 0xff:
		raise RuntimeError('CCC value cannot be encoded in current encoding')
	return out
def _ParseDecomposition(line: str, shift: int) -> tuple[int]|None:
	# check if the line defines any decomposition rules
	if len(line) == 0:
		return None

	# check if the rule starts with a formatting tag, in which case it is used for the compatibility-mapping and can be ignored
	if line[0] == '<':
		for tag in ['<font>', '<noBreak>', '<initial>', '<medial>', '<final>', '<isolated>', '<circle>', '<super>',
			  '<sub>', '<vertical>', '<wide>', '<narrow>', '<small>', '<square>', '<fraction>', '<compat>']:
			if line.startswith(tag):
				return None
		raise RuntimeError('Unknown tag encountered in decomposition mapping')

	# parse the list of single codepoint-values
	values = tuple(int(v, 16) for v in line.split(' '))

	# check if the size is valid
	if len(values) > 2:
		raise RuntimeError('Value count unexpected')
	return (len(values) << shift,) + values
def _ApplyCorrection(r: list[Range], c: int, v: tuple[int]) -> list[Range]:
	val = Ranges.lookup(r, c)
	if val is None:
		raise RuntimeError('Unable to apply normalization correction')
	if len(val) == 1 and val[0] == v[0]:
		return Ranges.merge(r, [Range(c, c, v[1])], lambda _, b: b)
	return r
def _TranslateComposition(v: tuple[int], shift: int, sizeMask: int) -> tuple[int]:
	count = len(v) // 2
	if count > sizeMask:
		raise RuntimeError('Too few size-bits to encode compositions')
	return (count << shift,) + v
def MakeNormalizationLookup(outPath: str, config: SystemConfig) -> None:
	# parse the relevant files
	unicodeData = ParsedFile(config.mapping['UnicodeData'], True)
	derivedNorm = ParsedFile(config.mapping['DerivedNormalizationProps'], False)
	normCorrections = ParsedFile(config.mapping['NormalizationCorrections'], False)

	# write all maps functions to the file
	with GeneratedFile(outPath, config) as file:
		# encoding:
		#	uint8_t: ccc-value
		#	uint8_t: len-decomposition (0 => no decomposition; directly following first value)
		#	uint8_t: num-compositions (pairs of combining-char and matching composition-char; must always be pair, hence count divided by two)
		#	uint1_t: isHangulSyllableComposition
		#	uint1_t: isHangulSyllableL
		#	uint1_t: isHangulSyllableV
		#	uint1_t: isHangulSyllableT
		#	uint1_t: isNormExclusion
		#
		#	all values are absolute as many compositions/decompositions to not benefit from relative values, as they might be partially static and dynamic

		# parse the canonical combining class values (https://www.unicode.org/reports/tr44/#Canonical_Combining_Class)
		cccRanges = unicodeData.values(lambda fs: _ParseCCC(fs[2]))

		# parse the exclusion flags (https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#page=70)
		exclusionFlag: int = 0x01 << 28
		excludedRanges = derivedNorm.values(lambda fs: exclusionFlag if fs[0] == 'Full_Composition_Exclusion' else None)
		excludedSet: set = set()
		for r in excludedRanges:
			for c in range(r.first, r.last + 1):
				excludedSet.add(c)

		# parse the hangulsyllable flags (https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#page=75)
		hangulSylComposition: int = 0x01 << 24
		hangulSyllableL: int = 0x01 << 25
		hangulSyllableV: int = 0x01 << 26
		hangulSyllableT: int = 0x01 << 27
		hangulSylRanges = Ranges.fromConflictingRawList([
			Range(0xac00, 0xac00 + 11172 - 1, hangulSylComposition), Range(0x1100, 0x1100 + 19 - 1, hangulSyllableL),
			Range(0x1161, 0x1161 + 21 - 1, hangulSyllableV), Range(0x11a7, 0x11a7 + 28 - 1, hangulSyllableT)
		], lambda a, b: (a[0] | b[0],))

		# fetch the decomposition mapping, but ignoring compatibility-mapping [length is guaranteed to be in 0..2] (https://www.unicode.org/reports/tr44/#Character_Decomposition_Mappings)
		decompSizeShift: int = 8
		decompRanges = unicodeData.values(lambda fs: _ParseDecomposition(fs[4], decompSizeShift))

		# apply all normalization-corrections where necessary
		for r in normCorrections.values(lambda fs: (int(fs[0], 16), int(fs[1], 16))):
			for c in range(r.first, r.last + 1):
				decompRanges = _ApplyCorrection(decompRanges, c, r.values)

		# collect all of the composition mappings [Primary Composites: must all be exactly length two; Must not be excluded] (Starter => [(CombiningChar,ComposedChar)])
		compSizeShift: int = 16
		compSizeMask: int = 0xff
		compRanges: list[Range] = []
		for r in decompRanges:
			if len(r.values) < 3:
				continue
			for c in range(r.first, r.last + 1):
				if c not in excludedSet:
					compRanges.append(Range(r.values[1], r.values[1], (r.values[2], c)))
		compRanges = Ranges.fromConflictingRawList(compRanges, lambda a, b: a + b)
		compRanges = Ranges.translate(compRanges, lambda _, v: _TranslateComposition(v, compSizeShift, compSizeMask))

		# merge the ranges together
		normRanges = Ranges.merge(cccRanges, excludedRanges, lambda a, b: (a[0] | b[0],))
		normRanges = Ranges.merge(normRanges, hangulSylRanges, lambda a, b: (a[0] | b[0],))
		normRanges = Ranges.merge(normRanges, decompRanges, lambda a, b: (a[0] | b[0],) + b[1:])
		normRanges = Ranges.merge(normRanges, compRanges, lambda a, b: (a[0] | b[0],) + a[1:] + b[1:])

		# create the function to encode the normalization properties (requires less memory than separate lookups)
		_type32: LookupType = LookupType.intType(0, 'uint32_t')
		_gen: CodeGen = file.next('Normalization', 'Lookup normalization properties (Decomposition, Composition, CCC, NormFlags)')
		_gen.addConstInt(_type32, 'NormIsHSComposition', hangulSylComposition)
		_gen.addConstInt(_type32, 'NormIsHSTypeL', hangulSyllableL)
		_gen.addConstInt(_type32, 'NormIsHSTypeV', hangulSyllableV)
		_gen.addConstInt(_type32, 'NormIsHSTypeT', hangulSyllableT)
		_gen.addConstInt(_type32, 'NormIsExcluded', exclusionFlag)
		_gen.addConstInt(_type32, 'NormCCCMask', 0xff)
		_gen.addConstInt(_type32, 'NormDecompShift', decompSizeShift)
		_gen.addConstInt(_type32, 'NormDecompMask', 0xff)
		_gen.addConstInt(_type32, 'NormDecompNone', 0)
		_gen.addConstInt(_type32, 'NormCompShift', compSizeShift)
		_gen.addConstInt(_type32, 'NormCompMask', compSizeMask)
		_gen.addConstInt(_type32, 'NormHSSBase', 0xac00)
		_gen.addConstInt(_type32, 'NormHSLBase', 0x1100)
		_gen.addConstInt(_type32, 'NormHSVBase', 0x1161)
		_gen.addConstInt(_type32, 'NormHSTBase', 0x11a7)
		_gen.addConstInt(_type32, 'NormHSLCount', 19)
		_gen.addConstInt(_type32, 'NormHSVCount', 21)
		_gen.addConstInt(_type32, 'NormHSTCount', 28)
		_gen.addConstInt(_type32, 'NormHSNCount', 588)
		_gen.listFunction('GetNormalization', _type32, normRanges, True, CodeGenConfig(CodeGenIndirect(), CodeGenDensityIfElse(1/2)))


# setup the current path
os.chdir(os.path.split(os.path.abspath(__file__))[0])
doTests: bool = ('--tests' in sys.argv)
doRefresh: bool = ('--refresh' in sys.argv)
doProperty: bool = ('--property' in sys.argv)
doCase: bool = ('--case' in sys.argv)
doSegment: bool = ('--segment' in sys.argv)
doNormalization: bool = ('--normal' in sys.argv)
print('Hint: use --refresh to download already cached files again')
print('Hint: use --tests to generate test-source-code')
print('Hint: use --property to generate property-code')
print('Hint: use --case to generate case-code')
print('Hint: use --segment to generate segmentation-code')
print('Hint: use --normal to generate normalization-code')

# check if the files need to be downloaded and extract the version and date-time
generatedURLOrigin = 'https://www.unicode.org/Public/UCD/latest'
generatedVersion, generatedMapping, testPath = DownloadUCDFiles(doRefresh, (doProperty or doCase or doSegment or doSegment), doTests, generatedURLOrigin)
generatedDateTime = datetime.datetime.today().strftime('%Y-%m-%d %H:%M')
systemConfig = SystemConfig(generatedURLOrigin, generatedVersion, generatedDateTime, generatedMapping, ['cinttypes', 'utility', 'algorithm'])

# generate the test files
if doTests:
	CreateSeparatorTestFile(testPath + 'test-words.h', systemConfig.mapping['WordBreakTest'], 'Word')
	CreateSeparatorTestFile(testPath + 'test-graphemes.h', systemConfig.mapping['GraphemeBreakTest'], 'Grapheme')
	CreateSeparatorTestFile(testPath + 'test-sentences.h', systemConfig.mapping['SentenceBreakTest'], 'Sentence')
	CreateSeparatorTestFile(testPath + 'test-lines.h', systemConfig.mapping['LineBreakTest'], 'Line')
	CreateNormalizationTestFile(testPath + 'test-normalization.h', systemConfig.mapping['NormalizationTest'])
	CreateEmojiTestFile(testPath + 'test-emoji.h', systemConfig.mapping['emoji-test'])

# generate the actual files
if doProperty:
	MakePropertyLookup('unicode-property.h', systemConfig)
if doCase:
	MakeCasingLookup('unicode-casing.h', systemConfig)
if doSegment:
	MakeSegmentationLookup('unicode-segmentation.h', systemConfig)
if doNormalization:
	MakeNormalizationLookup('unicode-normalization.h', systemConfig)
