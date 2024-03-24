import io
import urllib.request
import os
import sys
import datetime

# a range consists of: (inclusiveStart, inclusiveEnd, valueIndex, chars)
class Range:
	def __init__(self, start, end, valIndex, chars = None) -> None:
		self.start = start
		self.end = end
		self.valIndex = valIndex
		self.chars = chars if chars is not None else (self.end - self.start + 1)
	def __repr__(self) -> str:
		return f'[{self.start:05x}-{self.end:05x}/{self.chars}]:{self.valIndex}'
	def canMerge(self, right: 'Range', shallow: bool) -> bool:
		if shallow:
			return self.valIndex == right.valIndex
		return self.end + 1 == right.start and self.valIndex == right.valIndex
	def merge(self, right: 'Range') -> 'Range':
		return Range(self.start, right.end, self.valIndex, self.chars + right.chars)
	def split(self, size: int) -> tuple['Range', 'Range']:
		left = Range(self.start, self.start + size - 1, self.valIndex)
		right = Range(self.start + size, self.end, self.valIndex)
		return (left, right)
	def span(self) -> int:
		return (self.end - self.start + 1)

# ranges are lists of range-objects, which must not overlap with (if the have different valueIndices)
# ranges will automatically be sorted and merged with neighboring ranges

# a config must consist of: (fnName, varName, varAccName, typeName, dynLookup, valMap, defValue)
#	fnName: name of the function
#	varName: name to be used to define functions/buffers
#	varAccName: name to be used to access internal functions/buffers
#	typeName: the name of the type to be used
#	dynLookup: function to be called to convert value-index to type (i.e. static_cast for enums)
# 	valMap: map value-index to static value as [0, len(valueMap) - 1]
# 	defValue: value-index of default-value
class Config:
	def __init__(self, fnName, varName, varAccName, typeName, dynLookup, valMap, defValue) -> None:
		self.fnName = fnName
		self.varName = varName
		self.varAccName = varAccName
		self.typeName = typeName
		self.dynLookup = dynLookup
		self.valMap = valMap
		self.defValue = defValue
	def copy(self, add) -> 'Config':
		return Config(f'{self.varName}{add}', f'{self.varName}{add}', f'{self.varAccName}{add}', self.typeName, self.dynLookup, self.valMap, self.defValue)

def SplitRange(ranges: list[Range], startOfOther: int) -> tuple[list[Range], list[Range]]:
	left: list[Range] = []
	right: list[Range] = []

	# copy the ranges to the left until the cut-off point has been reached
	for i in range(len(ranges)):
		if ranges[i].start >= startOfOther:
			right.append(ranges[i])
		elif ranges[i].end < startOfOther:
			left.append(ranges[i])
		else:
			l, r = ranges[i].split(startOfOther - ranges[i].start)
			left.append(l)
			right.append(r)
	return (left, right)

def SortAndMergeRanges(ranges: list[Range]) -> list[Range]:
	# sort the range and check if the list contains overlapping conflicting properties
	ranges, i = sorted(ranges, key=lambda r : r.start), 0
	while i + 1 < len(ranges):
		if ranges[i + 1].start > ranges[i].end:
			i += 1
			continue
		if ranges[i + 1].valIndex != ranges[i].valIndex:
			raise RuntimeError(f'Range contains overlapping entries [{ranges[i].start:06x} - {ranges[i].end:06x}] and [{ranges[i + 1].start:06x} - {ranges[i + 1].end:06x}]')
	
		# merge the overlapping ranges
		ranges[i] = Range(min(ranges[i].start, ranges[i + 1].start), max(ranges[i].end, ranges[i + 1].end), ranges[i].valIndex)
		ranges = ranges[:i + 1] + ranges[i + 2:]

	# merge neighboring ranges of the same type
	merged = [ranges[0]]
	for i in range(1, len(ranges)):
		if merged[-1].canMerge(ranges[i], False):
			merged[-1] = merged[-1].merge(ranges[i])
		else:
			merged.append(ranges[i])
	return merged

def InsertDefValues(ranges: list[Range], defValue: int, leftEdge: int, rightEdge: int|None) -> list[Range]:
	# check if the lower edge needs to be flooded
	if leftEdge < ranges[0].start:
		ranges = [Range(leftEdge, ranges[0].start - 1, defValue)] + ranges
	
	# check if the upper edge needs to be flooded or if just a single placeholder needs to be added
	if rightEdge is None:
		ranges.append(Range(ranges[-1].end + 1, ranges[-1].end + 1, defValue))
	elif rightEdge > ranges[-1].end:
		ranges.append(Range(ranges[-1].end + 1, rightEdge, defValue))

	# patch all intermediate holes
	index = 0
	while index + 1 < len(ranges):
		if ranges[index].end + 1 != ranges[index + 1].start:
			ranges = ranges[:index + 1] + [Range(ranges[index].end + 1, ranges[index + 1].start - 1, defValue)] + ranges[index + 1:]
		else:
			index += 1
	return ranges

def CheckSizeAndDropDef(ranges: list[Range], defValue) -> list[Range]:
	# copy the ranges over and check if they need to be broken apart to fit into the 16bit (size will be decreased by one to allow 65536 to fit)
	out: list[Range] = []
	for i in range(len(ranges)):
		# check if the range can be ignored
		if ranges[i].valIndex == defValue:
			continue
		
		# add the next range and split it as long as necessary
		out.append(ranges[i])
		while out[-1].span() > 0x10000:
			out[-1], r = out[-1].split(0x10000)
			out.append(r)
	return out

def MakeCharString(val: int, valIsChar: bool) -> str:
	if not valIsChar or val >= 0xffff or (val >= 0xd800 and val <= 0xdfff):
		return f'0x{val:05x}'
	if val >= 0x80:
		return f'U\'\\u{val:04x}\''
	return f'U{repr(chr(val))}'

def WriteBufferOut(file: io.TextIOWrapper, type: str, name: str, data: list[int], largeAsHex) -> None:
	# slightly align the count to get a closer resembles of rectangles
	valsPerLine = 24 if largeAsHex else 32
	estimatedLines = (len(data) + valsPerLine - 1) // valsPerLine
	valsPerLine = (len(data) + estimatedLines - 1) // estimatedLines
	
	# write the header and the data out
	file.write(f'\tstatic constexpr {type} {name}[{len(data)}] = {{\n\t\t')
	for i in range(len(data)):
		if i > 0:
			file.write(f',{"\n\t\t" if (i % valsPerLine) == 0 else ""}')
		file.write(f' 0x{data[i]:04x}' if largeAsHex else f'{data[i]:3}')

	# close the buffer-string
	file.write('\n\t};\n')

def DensityClustering(ranges: list[Range]) -> None:
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
	return clusters

def WriteCodeAndBufferLookup(file: io.TextIOWrapper, ranges: list[Range], boundLeft: int, boundRight: int, binSearchLookup: bool, config: Config) -> bool:
	# fill all slots in the range with the default-value
	ranges = InsertDefValues(ranges, config.defValue, boundLeft, boundRight)
		
	# perform density-clustering to detect if buffers need to be created
	clusters = DensityClustering(ranges)
	indexStartOfClusters = len(config.valMap)

	# check if only a single large cluster has been produced in which case a direct buffer could be the alternative to this function
	if len(clusters) == 1 and clusters[0][0] == 0 and clusters[0][1] == len(ranges) - 1 and binSearchLookup:
		return False

	# write the cluster-buffers out and merge the ranges together (will only hold value-indices, therefore uint8_t is enough)
	shift = 0
	for i in range(len(clusters)):
		data = []
		for r in range(clusters[i][0], clusters[i][1] + 1):
			data += [ranges[r - shift].valIndex] * ranges[r - shift].span()
		WriteBufferOut(file, 'uint8_t', f'{config.varName}Buf{i}', data, False)

		# merge the ranges to a single large range and assign a new value-index to indicate its type
		index, count = clusters[i][0] - shift, clusters[i][1] - clusters[i][0] + 1
		f, b = ranges[index], ranges[index + count - 1]
		ranges[index] = Range(f.start, b.end, indexStartOfClusters + i)
		ranges = ranges[:index + 1] + ranges[index + count:]
		shift += count

	# count the number of ranges and characters per value-index [ranges, chars]
	totalValueIndices = indexStartOfClusters + len(clusters)
	incomplete = [[0, 0] for _ in range(totalValueIndices)]
	for i in range(len(ranges)):
		incomplete[ranges[i].valIndex][0] += 1
		incomplete[ranges[i].valIndex][1] += ranges[i].chars

	# add the function to actually lookup the value
	varName, usingChars = ("index" if binSearchLookup else "cp"), not binSearchLookup
	file.write(f'\tinline constexpr {config.typeName} {config.fnName}({"size_t" if binSearchLookup else "char32_t"} {varName}) {{\n')

	# iterate until all ranges have been processed/inserted
	while len(ranges) > 0:
		# look for the value-index with the largest number of characters, which consists of the fewest number of ranges
		bestIndex, bestRanges, bestChars = 0, 0, 0
		for i in range(totalValueIndices):
			ra, ch = incomplete[i]
			if ra != 0 and (bestRanges == 0 or bestRanges > ra or (bestRanges == ra and bestChars < ch)):
				bestIndex, bestRanges, bestChars = i, ra, ch
		
		# mark the value-index as consumed, as all corresponding ranges will now be processed
		incomplete[bestIndex] = [0, 0]

		# extract all ranges selected by the best value-index and merge the remaining ranges
		actual: list[Range] = []
		index = 0
		while index < len(ranges):
			if ranges[index].valIndex != bestIndex:
				index += 1
				continue
			actual.append(ranges[index])
			ranges = ranges[:index] + ranges[index + 1:]
			
			# check if the previous and next range can be merged (cannot be one of the selected types)
			if index > 0 and index < len(ranges) and ranges[index - 1].canMerge(ranges[index], True):
				ranges[index - 1] = ranges[index - 1].merge(ranges[index])
				ranges = ranges[:index] + ranges[index + 1:]
				incomplete[ranges[index - 1].valIndex][0] -= 1
		
		# check which side of the outer bounds of the actual ranges need to be checked
		checkLeft, checkRight = (len(ranges) > 0 and ranges[0].start < actual[0].start), (len(ranges) > 0 and ranges[-1].end > actual[-1].end)
		
		# check if this is a cluster-entry, in which case the buffer needs to be dereferenced (there can only be one range per cluster-entry)
		if bestIndex >= indexStartOfClusters:
			start, end = actual[0].start, actual[0].end

			# check which bounds need to be checked (closed-side can be ignored, as it would in turn ensure at least one range lies to the side of the buffer)
			if checkLeft or checkRight:
				file.write(f'\t\tif (')
				file.write(f'{varName} >= {MakeCharString(start, usingChars)}' if checkLeft else '')
				file.write(' && ' if (checkLeft and checkRight) else '')
				file.write(f'{varName} <= {MakeCharString(end, usingChars)}' if checkRight else '')
				file.write(f')\n\t\t\treturn ')
			else:
				file.write(f'\t\treturn ')

			# add the dereferencing
			file.write(f'{config.dynLookup}({config.varAccName}Buf{i - indexStartOfClusters}[{varName}')
			if start > 0:
				file.write(f' - {start}')
			file.write(']);\n')
			continue

		# check if these are the last ranges, in which case no if-statement is necessary
		if len(ranges) == 0:
			file.write(f'\t\treturn {config.valMap[bestIndex]};\n')
			continue

		# write the if-statement out and potentially break it, in case it could become an overlong row
		index = 0
		while index < len(actual):
			# check if the if-statement should be broken in two (if it has too many ranges)
			count = len(actual) - index
			if count >= 8:
				count = 4
			elif count > 4:
				count = (count + 1) // 2
		
			# add the next if-statement
			file.write(f'\t\tif (')

			# iterate over the ranges to be tested and add them to the condition (check greater/less equal before equality, to ensure open sides are handled properly)
			for i in range(index, index + count):
				if i > index:
					file.write(' || ')
				if not checkLeft and i == 0:
					file.write(f'{varName} <= {MakeCharString(actual[i].end, usingChars)}')
				elif not checkRight and i + 1 == len(actual):
					file.write(f'{varName} >= {MakeCharString(actual[i].start, usingChars)}')
				elif actual[i].chars == 1:
					file.write(f'{varName} == {MakeCharString(actual[i].start, usingChars)}')
				elif actual[i].chars == 2:
					file.write(f'{varName} == {MakeCharString(actual[i].start, usingChars)} || {varName} == {MakeCharString(actual[i].end, usingChars)}')
				elif count > 1:
					file.write(f'({varName} >= {MakeCharString(actual[i].start, usingChars)} && {varName} <= {MakeCharString(actual[i].end, usingChars)})')
				else:
					file.write(f'{varName} >= {MakeCharString(actual[i].start, usingChars)} && {varName} <= {MakeCharString(actual[i].end, usingChars)}')
			index += count

			# add the end of the if-statement and the return of the value
			file.write(f')\n\t\t\treturn {config.valMap[bestIndex]};\n')

	# write the tail of the function
	file.write('\t}\n')
	return True

def WriteBinarySearchLookup(file: io.TextIOWrapper, ranges: list[Range], config: Config) -> None:
	# cleanup the ranges used internally to adhere to the size-constraints of 16 bits (larger by one, as values are
	# stored decreased by one) and remove default-values, as the binary-search is only interested in the other values
	ranges = CheckSizeAndDropDef(ranges, config.defValue)

	# check if there are more than two values, in which case a value-map is required
	hasLookupFn = False
	if len(config.valMap) > 2:
		tempRanges = [Range(i, i, ranges[i].valIndex) for i in range(len(ranges))]

		# check if a code/buffer lookup can be used and otherwise create the buffer
		if WriteBoundedLookup(file, tempRanges, 0, tempRanges[-1].end, True, config.copy(f'Value')):
			hasLookupFn = True
		else:
			WriteBufferOut(file, 'uint8_t', f'{config.varName}Value', [v.valIndex for v in ranges], False)

	# write the range-sizes, which map range index to its size (uint16_t is guaranteed to be enough, but store the size smaller by 1 to allow 65536 to be written)
	if all(r.span() <= 256 for r in ranges):
		WriteBufferOut(file, 'uint8_t', f'{config.varName}Size', [r.span() - 1 for r in ranges], False)
	else:
		WriteBufferOut(file, 'uint16_t', f'{config.varName}Size', [r.span() - 1 for r in ranges], False)		

	# write the range-starts, which map range index to its beginning (as uint32_t as entries greater than 0xffff exist)
	if all(r.start <= 65536 for r in ranges):
		WriteBufferOut(file, 'uint16_t', f'{config.varName}Start', [r.start for r in ranges], True)
	else:
		WriteBufferOut(file, 'uint32_t', f'{config.varName}Start', [r.start for r in ranges], True)

	# add the function to actually lookup the type
	file.write(f'\tinline constexpr {config.typeName} {config.fnName}(char32_t cp) {{\n')

	# add the binary search for the matching range
	file.write(f'\t\tsize_t left = 0, right = {len(ranges) - 1};\n')
	file.write('\t\twhile (left < right) {\n')
	file.write('\t\t\tsize_t center = (left + right + 1) / 2;\n')
	file.write(f'\t\t\tif (cp < {config.varAccName}Start[center])\n')
	file.write('\t\t\t\tright = center - 1;\n')
	file.write('\t\t\telse\n')
	file.write('\t\t\t\tleft = center;\n')
	file.write('\t\t}\n')

	# add the check if the codepoint lies within the found range
	file.write(f'\t\tif (cp - {config.varAccName}Start[left] > {config.varAccName}Size[left])\n')
	file.write(f'\t\t\treturn {config.valMap[config.defValue]};\n')

	# either add the call to the type-lookup or add the array index/return the static value
	if len(config.valMap) <= 2:
		file.write(f'\t\treturn {config.valMap[1 - config.defValue]};\n')
	elif hasLookupFn:
		file.write(f'\t\treturn {config.varAccName}Value(left);\n')
	else:
		file.write(f'\t\treturn {config.dynLookup}({config.varAccName}Value[left]);\n')
	file.write('\t}\n')

def WriteBoundedLookup(file: io.TextIOWrapper, ranges: list[Range], boundLeft: int, boundRight: int, binSearchLookup: bool, config: Config) -> bool:
	# remove all default-ranges and sort the ranges (default entries will be added back if necessary, but
	# otherwise the range-checks might get confused as too many un-merged default values are encountered)
	ranges = SortAndMergeRanges(r for r in ranges if r.valIndex != config.defValue)
	
	# check if it is a simple lookup, in which case a code/buffer lookup is enough (low spread/few ranges)
	rangeCoveredChars, actualRangeChars = (ranges[-1].end - ranges[0].start + 1), sum(r.span() for r in ranges)
	if len(ranges) < 24 or (rangeCoveredChars <= 1.75 * actualRangeChars and rangeCoveredChars <= 3.0 * len(ranges)):
		return WriteCodeAndBufferLookup(file, ranges, boundLeft, boundRight, binSearchLookup, config)

	# check if its a binary-search sub-lookup, in which case only code-and-buffer lookups will be offered
	if binSearchLookup:
		return False

	# check if its an ascii-only range
	asciiRanges, ranges = SplitRange(ranges, 0x80)
	if len(ranges) == 0:
		WriteCodeAndBufferLookup(file, asciiRanges, boundLeft, boundRight, False, config)
		return True
	lowerRanges, upperRanges = SplitRange(ranges, 0x10000)

	# check if its a binary-search-only range
	if len(asciiRanges) == 0:
		if len(lowerRanges) == 0:
			WriteBinarySearchLookup(file, upperRanges, config)
			return True
		elif len(upperRanges) == 0:
			WriteBinarySearchLookup(file, lowerRanges, config)
			return True

	# setup the code and buffer-lookup for ascii
	if len(asciiRanges) > 0:
		WriteCodeAndBufferLookup(file, asciiRanges, 0x00, 0x7f, False, config.copy(f'Ascii'))

	# setup the lookups for the lower and upper
	if len(lowerRanges) > 0 and len(upperRanges) > 0:
		WriteBoundedLookup(file, lowerRanges, 0x80 if len(asciiRanges) > 0 else 0x00, 0x10000, False, config.copy(f'Low'))
		WriteBoundedLookup(file, upperRanges, 0x10000, boundRight, False, config.copy(f'High'))
	else:
		WriteBinarySearchLookup(file, ranges, config.copy(f'BinSearch'))

	# generate the glue code (i.e. the actual function)
	file.write(f'\tinline constexpr {config.typeName} {config.fnName}(char32_t cp) {{\n')
	file.write('\t\tif (cp < 0x80)\n')
	file.write(f'\t\t\treturn {config.varAccName}Ascii(cp);\n')
	if len(lowerRanges) > 0 and len(upperRanges) > 0:
		file.write('\t\tif (cp < 0x10000)\n')
		file.write(f'\t\t\treturn {config.varAccName}Low(cp);\n')
		file.write(f'\t\treturn {config.varAccName}High(cp);\n')
	else:
		file.write(f'\t\treturn {config.varAccName}BinSearch(cp);\n')
	file.write('\t}\n')
	return True

def WriteLookup(file: io.TextIOWrapper, ranges: list[Range], config: Config) -> None:
	print(f'Creating {config.fnName}...')
	WriteBoundedLookup(file, ranges, 0, 2**32 - 1, False, config)

def WriteEnumString(file: io.TextIOWrapper, enName, enMap):
	# sort the map to an array based on the actual stored integer indices
	map = sorted(enMap.keys(), key=lambda x: enMap[x])

	# write the enum out
	file.write(f'\tenum class {enName} : uint8_t {{\n')
	file.write(",\n".join([f'\t\t{key}' for key in map]))
	file.write('\n\t};\n')

def WriteComment(file: io.TextIOWrapper, msg: str, indent: bool):
	if indent:
		msg = msg.replace('\n', '\n\t*\t')
		file.write(f'\t/*\n\t*\t{msg}\n\t*/\n')
	else:
		msg = msg.replace('\n', '\n*\t')
		file.write(f'/*\n*\t{msg}\n*/\n')

def BeginFile(file: io.TextIOWrapper):
	file.write('#pragma once\n')
	file.write('\n')
	file.write('#include <cinttypes>\n')
	file.write('\n')
	WriteComment(file, 'This is an automatically generated file and should not be modified.\n'
			  + 'All data are based on the lastest information provided by the unicode character database.\n'
			  + 'Source: https://www.unicode.org/Public/UCD/latest\n'
			  + f'Generated: {datetime.datetime.today().strftime('%Y-%m-%d %H:%M')}', False)
	file.write('namespace cp::detail::gen {\n')

def EndFile(file: io.TextIOWrapper):
	file.write('}\n')

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
	begin, end = cp.split('..')
	return [missing, int(begin, 16), int(end, 16), fields]

def RawParseFile(path: str, legacyRanges: bool) -> list[tuple[int, int, bool, list[str]]]:
	# list of (begin, end, missing, fields)
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
			missing, begin, end, fields = parsed

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
			out.append((begin, end, missing, fields))
		if legacyState is not None:
			raise RuntimeError(f'Half-open legacy state encountered [{legacyState[0]:06x}]')
	return out

def ExtractProperties(parsed, relevantField: int, indexMap: map, findDefault: bool, failIfNotInMap: bool, filterSkip = None) -> tuple[list[Range], int|None]:
	# indexMap[None] matches the non-empty field string
	default, ranges = None, []

	# iterate over the parsed lines and validate them
	for (begin, end, missing, fields) in parsed:
		# check if the line can be ignored
		if len(fields) <= relevantField:
			if failIfNotInMap:
				raise RuntimeError(f'Too few fields in line [{begin:06x} - {end:06x}]')
			continue
		field = fields[relevantField].lower()

		# check if the value should be ignored
		if missing and not findDefault:
			continue
		if field in indexMap:
			valIndex = indexMap[field]
		elif (len(field) > 0 and None in indexMap):
			valIndex = indexMap[None]
		else:
			if failIfNotInMap:
				raise RuntimeError(f'Unknown field encountered [{field}]')
			continue
		
		# check if the value has been filtered out
		if filterSkip != None and filterSkip(fields):
			continue

		# check if this is the default value
		if missing:
			if default != None:
				raise RuntimeError('Multiple default values encountered')
			default = valIndex
			
		# add the range to the list
		else:
			ranges.append(Range(begin, end, valIndex))

	# check if a default value has been found
	if default is None and findDefault:
		raise RuntimeError(f'Default value has not been found')
	return ranges, default

def InvertMap(prefix: str, map: map) -> map:
	out = [0] * len(map)
	for key in map:
		out[map[key]] = f'{prefix}{key}'
	return out


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
	with open('generated/unicode-cp-query.h', 'w', encoding='ascii') as file:
		BeginFile(file)
		
		# write the unicode-test to the file
		unicodeRanges = [Range(0x110000, 2**32-1, 0)]
		unicodeRanges += ExtractProperties(unicodeData, 1, { 'cs': 0 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode General_Category is not cs (i.e. surrogate pairs) smaller than/equal to 0x10ffff', True)
		WriteLookup(file, unicodeRanges, Config('TestUnicode', 'Unicode', 'gen::Unicode', 'bool', 'bool', ['false', 'true'], 1))
		file.write('\n\n')

		# write the assigned-test to the file
		assignedRanges = ExtractProperties(unicodeData, 1, {
			'lu': 1, 'll': 1, 'lt': 1, 'lm': 1, 'lo': 1, 'mn': 1, 'mc': 1, 'me': 1, 'nd': 1, 'nl': 1, 'no': 1,
			'pc': 1, 'pd': 1, 'ps': 1, 'pe': 1, 'pi': 1, 'pf': 1, 'po': 1, 'sm': 1, 'sc': 1, 'sk': 1, 'so': 1,
			'zs': 1, 'zl': 1, 'zp': 1, 'cc': 1, 'cf': 1, 'cs': 0, 'co': 0, 'cn': 0
		}, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode General_Category is not Cn, Cs, Co (i.e. not assigned, surrogate pairs, private use)', True)
		WriteLookup(file, assignedRanges, Config('TestAssigned', 'Assigned', 'gen::Assigned', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')

		# write the ascii-test to the file
		asciiRanges = [Range(0, 0x7f, 1)]
		WriteComment(file, 'Automatically generated from: [cp <= 0x7f]', True)
		WriteLookup(file, asciiRanges, Config('TestAscii', 'Ascii', 'gen::Ascii', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')

		# write the alpha-test to the file
		alphaRanges = [Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)]
		WriteComment(file, 'Automatically generated from: [a-zA-Z]', True)
		WriteLookup(file, alphaRanges, Config('TestAlpha', 'Alpha', 'gen::Alpha', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')

		# write the radix-getter to the file
		radixRanges = [Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)]
		WriteComment(file, 'Automatically generated from: [0-9a-zA-Z] mapped to [0-35] and rest to 0xff', True)
		WriteLookup(file, radixRanges, Config('GetRadix', 'Radix', 'gen::Radix', 'uint8_t', 'uint8_t', [f'0x{i:02x}' for i in range(256)], 0xff))
		file.write('\n\n')

		# write the digit-getter to the file (https://www.unicode.org/reports/tr44/#Numeric_Type)
		digitRanges = ExtractProperties(unicodeData, 5, { str(i): i for i in range(10) }, False, False)[0]
		digitRanges += ExtractProperties(unicodeData, 6, { str(i): 0xf0 for i in range(10) }, False, False, lambda f: f[5] != '')[0]
		digitRanges += ExtractProperties(unicodeData, 7, { None: 0xf1 }, False, False, lambda f: f[5] != '' or f[6] != '')[0]
		WriteComment(file, 'Automatically generated from: Unicode: Numeric_Type=Decimal: [0-9]; Numeric_Type=Digit: [0xf0]; Numeric_Type=Numeric: [0xf1]; rest [0xff]', True)
		WriteLookup(file, digitRanges, Config('GetDigit', 'Digit', 'gen::Digit', 'uint8_t', 'uint8_t', [f'0x{i:02x}' for i in range(256)], 0xff))
		file.write('\n\n')

		# write the whitespace-test to the file (https://www.unicode.org/reports/tr44/#White_Space)
		whiteSpaceRanges = ExtractProperties(propList, 0, { 'white_space': 1 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode White_Space property', True)
		WriteLookup(file, whiteSpaceRanges, Config('TestWhiteSpace', 'WhiteSpace', 'gen::WhiteSpace', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')
		
		# write the control-test to the file (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
		controlRanges = ExtractProperties(unicodeData, 1, { 'cc': 1 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode General_Category is cc (i.e. C0, C1)', True)
		WriteLookup(file, controlRanges, Config('TestControl', 'Control', 'gen::Control', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')
		
		# write the letter-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic)
		letterRanges = ExtractProperties(derivedProperties, 0, { 'alphabetic': 1 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode derived property Alphabetic', True)
		WriteLookup(file, letterRanges, Config('TestLetter', 'Letter', 'gen::Letter', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')
		
		# write the alpha-num-test to the file (https://www.unicode.org/reports/tr44/#Alphabetic + https://www.unicode.org/reports/tr44/#Numeric_Type)
		alnumRanges = ExtractProperties(derivedProperties, 0, { 'alphabetic': 1 }, False, False)[0]
		alnumRanges += ExtractProperties(unicodeData, 5, { str(i): 1 for i in range(10) }, False, False)[0]
		alnumRanges += ExtractProperties(unicodeData, 6, { str(i): 1 for i in range(10) }, False, False, lambda f: f[5] != '')[0]
		alnumRanges += ExtractProperties(unicodeData, 7, { None: 1 }, False, False, lambda f: f[5] != '' or f[6] != '')[0]
		WriteComment(file, 'Automatically generated from: Unicode derived property Alphabetic or Numeric_Type=Decimal,Digit,Numeric', True)
		WriteLookup(file, alnumRanges, Config('TestAlNum', 'AlNum', 'gen::AlNum', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n\n')
		
		# write the printable-enum to the file (https://en.wikipedia.org/wiki/Graphic_character)
		printableEnumName, printableEnumMap = 'PrintableType', { 'none': 0, 'printable': 1, 'printSpace': 2 }
		printableRanges = ExtractProperties(unicodeData, 1, { 
			'lu': 1, 'll': 1, 'lt': 1, 'lm': 1, 'lo': 1, 'mn': 1, 'mc': 1, 'me': 1, 'nd': 1, 'nl': 1, 'no': 1,
			'pc': 1, 'pd': 1, 'ps': 1, 'pe': 1, 'pi': 1, 'pf': 1, 'po': 1, 'sm': 1, 'sc': 1, 'sk': 1, 'so': 1,
			'zs': 2 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode General_Category is L*,M*,N*,P*,S* or optionally Zs', True)
		WriteEnumString(file, printableEnumName, printableEnumMap)
		printableEnumName = f'gen::{printableEnumName}'
		WriteLookup(file, printableRanges, Config('GetPrintable', 'Printable', 'gen::Printable', printableEnumName, f'static_cast<{printableEnumName}>', InvertMap(f'{printableEnumName}::', printableEnumMap), printableEnumMap['none']))
		file.write('\n\n')
		
		# write the cased-enum to the file (https://www.unicode.org/reports/tr44/#Cased)
		caseEnumName, caseEnumMap = 'CaseType', { 'none': 0, 'lowerCase': 1, 'upperCase': 2, 'titleCase': 3 }
		caseRanges = ExtractProperties(derivedProperties, 0, { 'lowercase': 1, 'uppercase': 2 }, False, False)[0]
		caseRanges += ExtractProperties(unicodeData, 1, { 'lt': 3 }, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode derived property Lowercase, Uppercase or General_Category Lt', True)
		WriteEnumString(file, caseEnumName, caseEnumMap)
		caseEnumName = f'gen::{caseEnumName}'
		WriteLookup(file, caseRanges, Config('GetCase', 'Case', 'gen::Case', caseEnumName, f'static_cast<{caseEnumName}>', InvertMap(f'{caseEnumName}::', caseEnumMap), caseEnumMap['none']))
		file.write('\n\n')
		
		# write the category-enum to the file (https://www.unicode.org/reports/tr44/#GC_Values_Table)
		categoryEnumName, categoryEnumMap = 'CategoryType', {
			'lu': 0, 'll': 1, 'lt': 2, 'lm': 3, 'lo': 4, 'mn': 5, 'mc': 6, 'me': 7, 'nd': 8, 'nl': 9, 'no': 10,
			'pc': 11, 'pd': 12, 'ps': 13, 'pe': 14, 'pi': 15, 'pf': 16, 'po': 17, 'sm': 18, 'sc': 19, 'sk': 20, 'so': 21,
			'zs': 22, 'zl': 23, 'zp': 24, 'cc': 25, 'cf': 26, 'cs': 27, 'co': 28, 'cn': 29
		}
		categoryRanges = ExtractProperties(unicodeData, 1, categoryEnumMap, False, False)[0]
		WriteComment(file, 'Automatically generated from: Unicode General_Category', True)
		WriteEnumString(file, categoryEnumName, categoryEnumMap)
		categoryEnumName = f'gen::{categoryEnumName}'
		WriteLookup(file, categoryRanges, Config('GetCategory', 'Category', 'gen::Category', categoryEnumName, f'static_cast<{categoryEnumName}>', InvertMap(f'{categoryEnumName}::', categoryEnumMap), categoryEnumMap['cn']))

		EndFile(file)




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
