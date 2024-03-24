import io

# ranges: must not overlap, list of range-elements
# ranges: will automatically be sorted and merged

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

def ParseFile(path, fieldCount, relevantField, indexMap, findDefault, failIfNotInMap, legacyRanges) -> tuple[list[Range], int|None]:
	default = None
	ranges = []

	# open the file for reading and iterate over its lines
	with open(path, 'r', encoding='utf-8') as file:
		legacyState = None
		for line in file:
			# parse the line
			parsed = ParseLine(line)
			if parsed is None:
				continue
			missing, begin, end, property = parsed

			# check if a legacy range has been started
			if legacyState is not None:
				if ', Last>' not in property[0] or property[0][:-7] != legacyState[1] or property[1:] != legacyState[2:]:
					raise RuntimeError(f'Legacy range not closed properly [{begin:06x}]')
				begin = legacyState[0]
				legacyState = None
			elif legacyRanges and ', First>' in property[0]:
				legacyState = [begin, property[0][:-8]] + property[1:]
				continue

			# check if the line can be ignored
			if len(property) < fieldCount:
				if failIfNotInMap:
					raise RuntimeError(f'Too few properties in line [{begin:06x} - {end:06x}]')
				continue
			property = property[relevantField].lower()

			# check if the value should be ignored
			if missing and not findDefault:
				continue
			if property not in indexMap:
				if failIfNotInMap:
					raise RuntimeError(f'Unknown property encountered [{property}]')
				continue
			property = indexMap[property]

			# check if this is the default value
			if missing:
				if default != None:
					raise RuntimeError('Multiple default values encountered')
				default = property
			
			# add the range to the list
			else:
				ranges.append(Range(begin, end, property))
		if legacyState is not None:
			raise RuntimeError(f'Half-open legacy state encountered [{legacyState[0]:06x}]')

	# check if a default value has been found
	if default is None and findDefault:
		raise RuntimeError(f'Default property has not been found')
	return ranges, default

def SortAndMergeRanges(ranges: list[Range]) -> list[Range]:
	# sort the range and check if the list contains overlapping properties
	ranges = sorted(ranges, key=lambda r : r.start)
	for i in range(len(ranges) - 1):
		if ranges[i + 1].start < ranges[i].end:
			raise RuntimeError(f'Range contains overlapping entries [{ranges[i].start:06x} - {ranges[i].end:06x}] and [{ranges[i + 1].start:06x} - {ranges[i + 1].end:06x}]')

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

def MakeCharString(val: int) -> str:
	if val >= 0xffff or (val >= 0x8d00 and val <= 0x8fff):
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

def DensityClustering(ranges: list[Range], rightSideClosed: bool) -> None:
	thresholdDensity, thresholdChars = 1.0 / 1.8, 32

	# initialize the cluster-map [(first, last, ranges, chars)]
	clusters = []
	for i in range(len(ranges)):
		clusters.append((i, i, 1, ranges[i].span()))

	# check if the right side is open, in which case the correpsonding cluster must not be considered
	if not rightSideClosed:
		clusters = clusters[:-1]
	
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

def WriteCodeAndBufferLookup(file: io.TextIOWrapper, ranges: list[Range], rightSideClosed: bool, abortOnSingleCluster: bool, config: Config) -> bool:
	# perform density-clustering to detect if buffers need to be created
	clusters = DensityClustering(ranges, rightSideClosed)
	indexStartOfClusters = len(config.valMap)

	# check if only a single large cluster has been produced in which case a direct buffer could be the alternative to this function
	if len(clusters) == 1 and clusters[0][0] == 0 and clusters[0][1] == len(ranges) - 1 and abortOnSingleCluster:
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
	file.write(f'\tinline constexpr {config.typeName} {config.fnName}(char32_t cp) {{\n')

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
				file.write(f'cp >= {MakeCharString(start)}' if checkLeft else '')
				file.write(' || ' if (checkLeft and checkRight) else '')
				file.write(f'cp <= {MakeCharString(end)}' if checkRight else '')
				file.write(f')\n\t\t\treturn ')
			else:
				file.write(f'\t\treturn ')

			# add the dereferencing
			file.write(f'{config.dynLookup}({config.varAccName}Buf{i - indexStartOfClusters}[cp')
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
					file.write(f'cp <= {MakeCharString(actual[i].end)}')
				elif not checkRight and i + 1 == len(actual):
					file.write(f'cp >= {MakeCharString(actual[i].start)}')
				elif actual[i].chars == 1:
					file.write(f'cp == {MakeCharString(actual[i].start)}')
				elif actual[i].chars == 2:
					file.write(f'cp == {MakeCharString(actual[i].start)} || cp == {MakeCharString(actual[i].end)}')
				elif count > 1:
					file.write(f'(cp >= {MakeCharString(actual[i].start)} && cp <= {MakeCharString(actual[i].end)})')
				else:
					file.write(f'cp >= {MakeCharString(actual[i].start)} && cp <= {MakeCharString(actual[i].end)}')
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
	file.write('\t\t\tsize_t center = (right - left) / 2;\n')
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

def WriteBoundedLookup(file: io.TextIOWrapper, ranges: list[Range], boundLeft: int, boundRight: int|None, binSearchLookup: bool, config: Config) -> bool:
	# remove all default-ranges and sort the ranges (default entries will be added back if necessary, but
	# otherwise the range-checks might get confused as too many un-merged default values are encountered)
	ranges = SortAndMergeRanges(r for r in ranges if r.valIndex != config.defValue)
	
	# check if it is a simple lookup, in which case a code/buffer lookup is enough (low spread/few ranges)
	rangeCoveredChars, actualRangeChars = (ranges[-1].end - ranges[0].start + 1), sum(r.span() for r in ranges)
	if len(ranges) < 24 or (rangeCoveredChars <= 1.75 * actualRangeChars and rangeCoveredChars <= 3.0 * len(ranges)):
		ranges = InsertDefValues(ranges, config.defValue, boundLeft, boundRight)
		return WriteCodeAndBufferLookup(file, ranges, boundRight is not None, binSearchLookup, config)

	# check if its a binary-search sub-lookup, in which case only code-and-buffer lookups will be offered
	if binSearchLookup:
		return False

	# check if its an ascii-only range
	asciiRanges, ranges = SplitRange(ranges, 0x80)
	if len(ranges) == 0:
		asciiRanges = InsertDefValues(asciiRanges, config.defValue, boundLeft, boundRight)
		WriteCodeAndBufferLookup(file, asciiRanges, False, boundRight is not None, config)
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
		asciiRanges = InsertDefValues(asciiRanges, config.defValue, 0, 0x7f)
		WriteCodeAndBufferLookup(file, asciiRanges, True, False, config.copy(f'Ascii'))

	# setup the lookups for the lower and upper
	if len(lowerRanges) > 0 and len(upperRanges) > 0:
		WriteBoundedLookup(file, lowerRanges, 0x80 if len(asciiRanges) > 0 else 0x00, 0x10000, False, config.copy(f'Low'))
		WriteBoundedLookup(file, upperRanges, 0x10000, None, False, config.copy(f'High'))
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
	WriteBoundedLookup(file, ranges, 0, None, False, config)

def WriteEnumString(file: io.TextIOWrapper, enName, enMap):
	# sort the map to an array based on the actual stored integer indices
	map = sorted(enMap.keys(), key=lambda x: enMap[x])

	# write the enum out
	file.write(f'\tenum class {enName} : uint8_t {{\n')
	file.write(",\n".join([f'\t\t{key}' for key in map]))
	file.write('\n\t};\n')

def BeginFile(file: io.TextIOWrapper):
	file.write('#pragma once\n')
	file.write('\n')
	file.write('#include <cinttypes>\n')
	file.write('\n')
	file.write('/*\n')
	file.write('*\tThis is an automatically generated file and should not be modified\n')
	file.write('*/\n')
	file.write('namespace str::cp::detail {\n')

def EndFile(file: io.TextIOWrapper):
	file.write('}\n')

def RemapRanges(ranges: list[Range], oldMap, newMap) -> list[Range]:
	# invert the map to revert the range value-indices
	inverse = [0] * len(oldMap)
	for key in oldMap:
		inverse[oldMap[key]] = key

	# map all ranges over
	remapped = []
	for i in range(len(ranges)):
		remapped.append(Range(ranges[i].start, ranges[i].end, newMap[inverse[ranges[i].valIndex]], ranges[i].chars))
	return remapped

def FilterRanges(ranges: list[Range], filterVal: int) -> list[Range]:
	return [r for r in ranges if r.valIndex != filterVal]

def InvertMap(prefix, map):
	out = [0] * len(map)
	for key in map:
		out[map[key]] = f'{prefix}{key}'
	return out






# IsAscii [<= 0x7f; bool]
# IsAlpha [a-zA-Z; bool]
# IsDigit [0-9a-zA-Z; 0-35, 0xff]
# IsWhitespace [PropList.txt: White_Space; bool]
# IsControl [C0 or C1; UnicodeData.txt: Cc]
# GetPrintable [UnicodeData.txt: Not C./Z.; printable, printSpace, none]
# GetCase [IsLowercase, IsUppercase, TitleCased, None] 


# IsLowercase [DerivedCoreProperties.txt: lowercase]
# IsUppercase [DerivedCoreProperties.txt: uppercase]
# ToLower
# ToUpper
# IsPunctuation
# IsLetter 
# GetCategory [UnicodeData.txt: ...]




def MakeCodepointTesting():
	# load the initial General_Category values (default value else 'cn')
	categoryMap = {
		'lu': 0, 'll': 1, 'lt': 2, 'lm': 3, 'lo': 4, 'mn': 5, 'mc': 6, 'me': 7, 'nd': 8, 'nl': 9, 'no': 10,
		'pc': 11, 'pd': 12, 'ps': 13, 'pe': 14, 'pi': 15, 'pf': 16, 'po': 17, 'sm': 18, 'sc': 19, 'sk': 20, 'so': 21,
		'zs': 22, 'zl': 23, 'zp': 24, 'cc': 25, 'cf': 26, 'cs': 27, 'co': 28, 'cn': 29
	}
	categoryRanges = ParseFile('ucd/UnicodeData.txt', 14, 1, categoryMap, False, True, True)[0]

	# prepare the ascii ranges (<= 0x7f)
	asciiRanges = [Range(0, 0x7f, 1)]

	# prepare the alpha-ranges [a-zA-Z]
	alphaRanges = [Range(ord('a'), ord('z'), 1), Range(ord('A'), ord('Z'), 1)]

	# prepare the digit-ranges [0-9a-zA-Z]
	digitRanges = [Range(ord('0') + i, ord('0') + i, i) for i in range(10)] + [Range(ord('a') + i, ord('a') + i, 10 + i) for i in range(26)] + [Range(ord('A') + i, ord('A') + i, 10 + i) for i in range(26)]

	# prepare the whitespace state (https://www.unicode.org/reports/tr44/#White_Space)
	whiteSpaceRanges = ParseFile('ucd/PropList.txt', 1, 0, { 'white_space': 1 }, False, False, False)[0]

	# prepare the control state (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
	controlRanges = RemapRanges(categoryRanges, categoryMap, {
		'lu': 0, 'll': 0, 'lt': 0, 'lm': 0, 'lo': 0, 'mn': 0, 'mc': 0, 'me': 0, 'nd': 0, 'nl': 0, 'no': 0,
		'pc': 0, 'pd': 0, 'ps': 0, 'pe': 0, 'pi': 0, 'pf': 0, 'po': 0, 'sm': 0, 'sc': 0, 'sk': 0, 'so': 0,
		'zs': 0, 'zl': 0, 'zp': 0, 'cc': 1, 'cf': 0, 'cs': 0, 'co': 0, 'cn': 0
	})

	# prepare the printable state (https://en.wikipedia.org/wiki/Graphic_character) 
	printableRanges = RemapRanges(categoryRanges, categoryMap, {
		'lu': 1, 'll': 1, 'lt': 1, 'lm': 1, 'lo': 1, 'mn': 1, 'mc': 1, 'me': 1, 'nd': 1, 'nl': 1, 'no': 1,
		'pc': 1, 'pd': 1, 'ps': 1, 'pe': 1, 'pi': 1, 'pf': 1, 'po': 1, 'sm': 1, 'sc': 1, 'sk': 1, 'so': 1,
		'zs': 2, 'zl': 0, 'zp': 0, 'cc': 0, 'cf': 0, 'cs': 0, 'co': 0, 'cn': 0
	})

	# prepare the cased state (https://www.unicode.org/reports/tr44/#Cased)
	caseRanges = ParseFile('ucd/DerivedCoreProperties.txt', 1, 0, {
		'lowercase': 1, 'uppercase': 2
	}, False, False, False)[0]
	caseRanges += FilterRanges(RemapRanges(categoryRanges, categoryMap, {
		'lu': 0, 'll': 0, 'lt': 3, 'lm': 0, 'lo': 0, 'mn': 0, 'mc': 0, 'me': 0, 'nd': 0, 'nl': 0, 'no': 0,
		'pc': 0, 'pd': 0, 'ps': 0, 'pe': 0, 'pi': 0, 'pf': 0, 'po': 0, 'sm': 0, 'sc': 0, 'sk': 0, 'so': 0,
		'zs': 0, 'zl': 0, 'zp': 0, 'cc': 0, 'cf': 0, 'cs': 0, 'co': 0, 'cn': 0
	}), 0)

	# write the state to the file
	with open('test-unicode.h', 'w', encoding='ascii') as file:
		BeginFile(file)
		
		# write the ascii-test to the file
		WriteLookup(file, asciiRanges, Config('TestAscii', 'Ascii', 'detail::Ascii', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n')

		# write the alpha-test to the file
		WriteLookup(file, alphaRanges, Config('TestAlpha', 'Alpha', 'detail::Alpha', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n')

		# write the digit-test to the file
		WriteLookup(file, digitRanges, Config('TestDigit', 'Digit', 'detail::Digit', 'uint8_t', 'uint8_t', [f'0x{i:02x}' for i in range(256)], 0xff))
		file.write('\n')

		# write the whitespace-test to the file
		WriteLookup(file, whiteSpaceRanges, Config('TestWhiteSpace', 'WhiteSpace', 'detail::WhiteSpace', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n')
		
		# write the control-test to the file
		WriteLookup(file, controlRanges, Config('TestControl', 'Control', 'detail::Control', 'bool', 'bool', ['false', 'true'], 0))
		file.write('\n')
		
		# write the printable-enum to the file
		printableEnumName, printableEnumMap = 'PrintableType', {
			'none': 0, 'printable': 1, 'printSpace': 2
		}
		WriteEnumString(file, printableEnumName, printableEnumMap)
		printableEnumName = f'detail::{printableEnumName}'
		WriteLookup(file, printableRanges, Config('GetPrintable', 'Printable', 'detail::Printable', printableEnumName, f'static_cast<{printableEnumName}>', InvertMap(f'{printableEnumName}::', printableEnumMap), 0))
		file.write('\n')
		
		# write the cased-enum to the file
		caseEnumName, caseEnumMap = 'CaseType', {
			'none': 0, 'lowerCase': 1, 'upperCase': 2, 'titleCase': 3
		}
		WriteEnumString(file, caseEnumName, caseEnumMap)
		caseEnumName = f'detail::{caseEnumName}'
		WriteLookup(file, caseRanges, Config('GetCase', 'Case', 'detail::Case', caseEnumName, f'static_cast<{caseEnumName}>', InvertMap(f'{caseEnumName}::', caseEnumMap), 0))
		
		EndFile(file)

def MakeGraphemeTypeMapping():
	graphemeEnumName, graphemeEnumMap = 'GraphemeType', {
		'other': 0, 'prepend': 1, 'cr': 2, 'lf': 3, 
		'control': 4, 'extend': 5, 'regional_indicator': 6, 'spacingmark': 7, 
		'l': 8, 'v': 9, 't': 10, 'lv': 11, 'lvt': 12, 'zwj': 13, 'extended_pictographic': 14
	}

	# parse the GraphemeBreakProperty.txt file to extract the grapheme-properties
	ranges, defValue = ParseFile('ucd/GraphemeBreakProperty.txt', 1, 0, graphemeEnumMap, True, True, False)
	if defValue != graphemeEnumMap['other']:
		raise RuntimeError('Default grapheme-value is expected to be [other]')
	
	# parse the emoji-data.txt file to extract the Extended_Pictographic property
	ranges += ParseFile('ucd/emoji-data.txt', 1, 0, {
		'extended_pictographic': graphemeEnumMap['extended_pictographic'] 
	}, False, False, False)[0]

	# open the output file and generate the code into it
	with open('grapheme-type.h', 'w', encoding='ascii') as file:
		BeginFile(file)
		
		# write the grapheme-enum to the file
		WriteEnumString(file, graphemeEnumName, graphemeEnumMap)
		WriteLookup(file, ranges, Config('LookupGraphemeType', 'Grapheme', 'detail::Grapheme', f'detail::{graphemeEnumName}', f'static_cast<detail::{graphemeEnumName}>', InvertMap(f'detail::{graphemeEnumName}::', graphemeEnumMap), 0))
		
		EndFile(file)




MakeCodepointTesting()
MakeGraphemeTypeMapping()
