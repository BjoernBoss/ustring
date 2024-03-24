# range: tuple of (inclusiveStart, inclusiveEnd, valueIndex)
# enMap: maps strings to [0, len(enMap) - 1]
# ranges: must not overlap, list of range-elements

# ranges will automatically be sorted and merged

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

def ParseFile(path, fieldCount, relevantField, indexMap, findDefault, failIfNotInMap, legacyRanges):
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
				ranges.append((begin, end, property))
		if legacyState is not None:
			raise RuntimeError(f'Half-open legacy state encountered [{legacyState[0]:06x}]')

	# check if a default value has been found
	if default is None and findDefault:
		raise RuntimeError(f'Default property has not been found')
	return ranges, default

def PrepareAndCleanRanges(ranges, defValue):
	# sort the range and check if the list contains overlapping properties
	ranges = sorted(ranges, key=lambda r : r[0])
	for i in range(len(ranges) - 1):
		if ranges[i + 1][0] < ranges[i][1]:
			raise RuntimeError(f'Range contains overlapping entries [{ranges[i][0]:06x} - {ranges[i][1]:06x}] and [{ranges[i + 1][0]:06x} - {ranges[i + 1][1]:06x}]')

	# add the front and back default values such that the entire range is covered [0, topValue]
	if ranges[0][0] > 0:
		ranges = [(0, ranges[0][0] - 1, defValue)] + ranges
	if ranges[-1][1] < 0x10ffff:
		ranges.append((ranges[-1][1] + 1, 0x10ffff, defValue))
	
	# patch all intermediate holes (no size restrictions, as they will be ensured later when merging the entries)
	index = 0
	while index + 1 < len(ranges):
		if ranges[index][1] + 1 != ranges[index + 1][0]:
			ranges = ranges[:index + 1] + [(ranges[index][1] + 1, ranges[index + 1][0] - 1, defValue)] + ranges[index + 1:]
		else:
			index += 1
	
	# merge neighboring ranges of the same type
	merged = [ranges[0]]
	for i in range(1, len(ranges)):
		if merged[-1][2] != ranges[i][2]:
			merged.append(ranges[i])
		else:
			merged[-1] = (merged[-1][0], ranges[i][1], merged[-1][2])
	return merged

def EnsureSizeConstraintsAndDropDefault(ranges, defValue):
	# copy the ranges over and check if they need to be broken apart to fit into the 16bit (size will be decreased by one to allow 65536 to fit)
	out = []
	for i in range(len(ranges)):
		# check if the range can be ignored
		if ranges[i][2] == defValue:
			continue
		offset = ranges[i][0]
		
		# add the next range and split it as long as necessary
		while offset <= ranges[i][1]:
			count = ranges[i][1] - offset + 1
			if count > 0x10000:
				count = 0x10000
			out.append((offset, offset + count - 1, ranges[i][2]))
			offset += count
	return out

def MakeCharString(val):
	if val >= 0xffff or (val >= 0x8d00 and val <= 0x8fff):
		return f'0x{val:05x}'
	if val >= 0x80:
		return f'U\'\\u{val:04x}\''
	return f'U{repr(chr(val))}'

def MakeCodeLookup(file, ranges, enNameElseBool, valMap, fnName):
	# count the number of ranges per property and characters per property and update the ranges to also contain their actual character-count
	incomplete = [[0, 0] for _ in range(len(valMap))]
	for i in range(len(ranges)):
		ranges[i] = (ranges[i][0], ranges[i][1], ranges[i][2], ranges[i][1] - ranges[i][0] + 1)
		incomplete[ranges[i][2]][0] += 1
		incomplete[ranges[i][2]][1] += ranges[i][3]
	
	# add the function to actually lookup the type
	file.write(f'\tinline constexpr {"bool" if enNameElseBool is None else enNameElseBool} {fnName}(char32_t cp) {{\n')

	# process all ranges until they have all been inserted
	while True:
		# look for the property with the largest number of characters, which consists of the fewest number of ranges
		best = 0
		for i in range(1, len(incomplete)):
			if incomplete[i][0] == 0:
				continue
			if incomplete[best][0] == 0 or incomplete[best][0] > incomplete[i][0] or (incomplete[best][0] == incomplete[i][0] and incomplete[best][1] < incomplete[i][1]):
				best = i
		incomplete[best] = [0, 0]

		# extract all ranges selected by the property and merge the remaining ranges
		actual, index, leftMost, rightMost = [], 0, False, False
		while index < len(ranges):
			if ranges[index][2] != best:
				index += 1
				continue
			actual.append(ranges[index])
			if index == 0:
				leftMost = True
			if index + 1 == len(ranges):
				rightMost = True

			# check if the previous and next range can be merged (cannot be one of the selected types)
			if index > 0 and index + 1 < len(ranges) and ranges[index - 1][2] == ranges[index + 1][2]:
				merged = (ranges[index - 1][0], ranges[index + 1][1], ranges[index - 1][2], ranges[index - 1][3] + ranges[index + 1][3])
				ranges = ranges[:index - 1] + [merged] + ranges[index + 2:]
				incomplete[merged[2]][0] -= 1
			else:
				ranges = ranges[:index] + ranges[index + 1:]
				index += 1

		# check if these are the last ranges, in which case no if-statement is necessary
		if len(ranges) == 0:
			if enNameElseBool is None:
				file.write(f'\t\treturn {"true" if valMap[best] > 0 else "false"};\n')
			else:
				file.write(f'\t\treturn {enNameElseBool}::{valMap[best]};\n')
			break

		consumed = 0
		while consumed < len(actual):
			# check if the if-statement should be broken in two (it too many ranges)
			count = len(actual) - consumed
			if count >= 8:
				count = 4
			elif count > 4:
				count = (count + 1) // 2
		
			# add the next if-statement
			file.write(f'\t\tif (')

			# iterate over the ranges to be tested and add them to the condition
			for i in range(consumed, consumed + count):
				if i > consumed:
					file.write(' || ')
				if actual[i][3] == 1:
					file.write(f'cp == {MakeCharString(actual[i][0])}')
				elif leftMost and i == 0:
					file.write(f'cp <= {MakeCharString(actual[i][1])}')
				elif rightMost and i + 1 == len(actual):
					file.write(f'cp >= {MakeCharString(actual[i][0])}')
				elif actual[i][3] == 2:
					file.write(f'cp == {MakeCharString(actual[i][0])} || cp == {MakeCharString(actual[i][1])}')
				elif len(actual) > 1:
					file.write(f'(cp >= {MakeCharString(actual[i][0])} && cp <= {MakeCharString(actual[i][1])})')
				else:
					file.write(f'cp >= {MakeCharString(actual[i][0])} && cp <= {MakeCharString(actual[i][1])}')
			consumed += count

			# add the end of the if-statement and the return of the value
			if enNameElseBool is None:
				file.write(f')\n\t\t\treturn {"true" if valMap[best] > 0 else "false"};\n')
			else:
				file.write(f')\n\t\t\treturn {enNameElseBool}::{valMap[best]};\n')

	# write the tail of the function
	file.write('\t}\n')

def MakeComplexLookup(file, ranges, defValue, enNameElseBool, valMap, fnName, subName, subNamespace):
	# split off the ascii characters for a speed-up
	asciiRanges = []
	while ranges[0][1] < 0x80:
		asciiRanges.append(ranges[0])
		ranges = ranges[1:]
	if ranges[0][0] < 0x80:
		asciiRanges.append((ranges[0][0], 0x7f, ranges[0][2]))
		ranges[0] = (0x80, ranges[0][1], ranges[0][2])
	
	# cleanup the ranges used internally to adhere to the size-constraints of 16 bits (larger by one, as values are
	# stored decreased by one) and remove default-values, as the binary-search is only interested in the other values
	ranges = EnsureSizeConstraintsAndDropDefault(ranges, defValue)

	# if the type is an enum, write the type-map, which maps ranges to their type (as uint8_t as the expanded enum would bloat the file)
	if enNameElseBool is not None:
		file.write(f'\tstatic constexpr uint8_t {subName}RangeType[{len(ranges)}] = {{\n\t\t')
		for i in range(len(ranges)):
			if i > 0:
				file.write(f',{"\n\t\t" if (i % 64) == 0 else ""}')
			file.write(str(ranges[i][2]))
		file.write('\n\t};\n')

	# write the range-sizes, which maps range index to its size (as uint16_t as it will be enough, but store the size smaller by 1 to allow 65536 to be written)
	file.write(f'\tstatic constexpr uint16_t {subName}RangeSize[{len(ranges)}] = {{\n\t\t')
	for i in range(len(ranges)):
		if i > 0:
			file.write(f',{"\n\t\t" if (i % 64) == 0 else ""}')
		file.write(str(ranges[i][1] - ranges[i][0]))
	file.write('\n\t};\n')

	# write the range-starts, which maps range index to its beginning (as uint32_t as entries greater than 0xffff exist)
	file.write(f'\tstatic constexpr uint32_t {subName}RangeStart[{len(ranges)}] = {{\n\t\t')
	for i in range(len(ranges)):
		if i > 0:
			file.write(f',{"\n\t\t" if (i % 32) == 0 else ""}')
		file.write(f'0x{ranges[i][0]:05x}')
	file.write('\n\t};\n')

	# check if an additional ascii-type lookup map needs to be added (as uint8_t like the normal type-map, or bool if boolean-ranges)
	highComplexity = (len(asciiRanges) >= 32)
	if highComplexity:
		file.write(f'\tstatic constexpr {"bool" if enNameElseBool is None else "uint8_t"} {subName}LookupAscii[128] = {{\n\t\t')
		index = 0
		for i in range(128):
			if i > 0:
				file.write(f',{"\n\t\t" if (i % 32) == 0 else ""}')
			if i > asciiRanges[index][1]:
				index += 1
			if enNameElseBool is None:
				file.write('true' if asciiRanges[index] else 'false')
			else:
				file.write(f'0x{asciiRanges[index][2]}')
		file.write('\n\t};\n')

	# otherwise add the function for the ascii-evaluation
	else:
		MakeCodeLookup(file, asciiRanges, enNameElseBool, valMap, f'{subName}LookupAscii')

	# add the function to actually lookup the type
	file.write(f'\tinline constexpr {"bool" if enNameElseBool is None else enNameElseBool} {fnName}(char32_t cp) {{\n')

	# check if the character is an ascii character and either add the ascii-lookup or the logic
	file.write(f'\t\t/* check if the codepoint is an ascii character and speed up its type-lookup */\n')
	file.write('\t\tif (cp < 0x80)\n')
	if not highComplexity:
		file.write(f'\t\t\treturn {subNamespace}::{subName}LookupAscii(cp);\n\n')
	elif enNameElseBool is None:
		file.write(f'\t\t\treturn {subNamespace}::{subName}LookupAscii[cp];\n\n')
	else:
		file.write(f'\t\t\treturn static_cast<{enNameElseBool}>({subNamespace}::{subName}LookupAscii[cp]);\n\n')
	
	# add the binary search for the matching range
	file.write(f'\t\t/* perform binary search for the matching range */\n')
	file.write(f'\t\tsize_t left = 0, right = {len(ranges) - 1};\n')
	file.write('\t\twhile (left < right) {\n')
	file.write('\t\t\tsize_t center = (right - left) / 2;\n')
	file.write(f'\t\t\tif (cp < {subNamespace}::{subName}RangeStart[center])\n')
	file.write('\t\t\t\tright = center - 1;\n')
	file.write('\t\t\telse\n')
	file.write('\t\t\t\tleft = center;\n')
	file.write('\t\t}\n\n')

	# add the check if the codepoint lies within the found range
	file.write(f'\t\t/* check if the codepoint lies within the found range (size is smaller by one to allow 65536 to be encoded as range-size) */\n')
	if enNameElseBool is None:
		if valMap[defValue] > 0:
			file.write(f'\t\treturn (cp - {subNamespace}::{subName}RangeStart[left] > {subNamespace}::{subName}RangeSize[left]);\n')
		else:
			file.write(f'\t\treturn (cp - {subNamespace}::{subName}RangeStart[left] <= {subNamespace}::{subName}RangeSize[left]);\n')
	else:
		file.write(f'\t\tif (cp - {subNamespace}::{subName}RangeStart[left] > {subNamespace}::{subName}RangeSize[left])\n')
		file.write(f'\t\t\treturn {enNameElseBool}::{valMap[defValue]};\n')
		file.write(f'\t\treturn static_cast<{enNameElseBool}>({subNamespace}::{subName}RangeType[left]);\n')
	file.write('\t}\n')

def WriteEnumLookup(file, ranges, defValue, enName, enMap, fnName, subName, subNamespace):
	defValue = enMap[defValue]
	ranges = PrepareAndCleanRanges(ranges, defValue)
	
	# invert the enum-map to generate the value-map
	valMap = [0] * len(enMap)
	for key in enMap:
		valMap[enMap[key]] = key

	# check if the complex lookup should be used
	if len(ranges) > 96:
		MakeComplexLookup(file, ranges, defValue, enName, valMap, fnName, subName, subNamespace)
	else:
		MakeCodeLookup(file, ranges, enName, valMap, fnName, False)

def WriteBinaryLookup(file, ranges, defValue, fnName, subName, subNamespace):
	ranges = PrepareAndCleanRanges(ranges, defValue)
	
	# setup the dummy value-map
	valMap = [0, 1]

	# check if the complex lookup should be used
	if len(ranges) > 96:
		MakeComplexLookup(file, ranges, defValue, None, valMap, fnName, subName, subNamespace)
	else:
		MakeCodeLookup(file, ranges, None, valMap, fnName)

def WriteEnumString(file, enName, enMap):
	# sort the map to an array based on the actual stored integer indices
	map = sorted(enMap.keys(), key=lambda x: enMap[x])

	# write the enum out
	file.write(f'\tenum class {enName} : uint8_t {{\n')
	file.write(",\n".join([f'\t\t{key}' for key in map]))
	file.write('\n\t};\n')

def BeginFile(file):
	file.write('#pragma once\n')
	file.write('\n')
	file.write('#include <cinttypes>\n')
	file.write('\n')
	file.write('/*\n')
	file.write('*\tThis is an automatically generated file and should not be modified\n')
	file.write('*/\n')
	file.write('namespace str::cp::detail {\n')

def EndFile(file):
	file.write('}\n')

def RemapRanges(ranges, oldMap, newMap):
	# invert the map to revert the range value-indices
	inverse = [0] * len(oldMap)
	for key in oldMap:
		inverse[oldMap[key]] = key

	# map all ranges over
	remapped = []
	for i in range(len(ranges)):
		remapped.append((ranges[i][0], ranges[i][1], newMap[inverse[ranges[i][2]]]))
	return remapped








# add ability to enforce lookup-map
# add ability to return integer types as well

# IsLowercase [DerivedCoreProperties.txt: lowercase]
# IsUppercase [DerivedCoreProperties.txt: uppercase]
# ToLower
# ToUpper
# IsAscii [<= 0x7f]
# IsAlpha [a-zA-Z]
# IsDigit [0-9a-zA-Z]
# IsWhitespace [PropList.txt: White_Space]
# IsPrintable [UnicodeData.txt: Not C./Z. excluding 0x20 U' ']
# IsControl [C0 or C1; UnicodeData.txt: Cc]
# IsPunctuation
# IsLetter 
# GetCased [IsLowercase?, IsUppercase?, UnicodeData.txt:Lt?, None] 
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
	asciiRanges = [(0, 0x7f, 1)]

	# prepare the alpha-ranges [a-zA-Z]
	alphaRanges = [(ord('a'), ord('z'), 1), (ord('A'), ord('Z'), 1)]

	# prepare the digit-ranges [0-9a-zA-Z]
	digitRanges = [(ord('0'), ord('9'), 1), (ord('a'), ord('z'), 1), (ord('A'), ord('Z'), 1)]

	# prepare the whitespace state (https://www.unicode.org/reports/tr44/#White_Space)
	whiteSpaceRanges = ParseFile('ucd/PropList.txt', 1, 0, { 'white_space': 1 }, False, False, False)[0]

	# prepare the printable state (https://en.wikipedia.org/wiki/Graphic_character) 
	printableRanges = RemapRanges(categoryRanges, categoryMap, {
		'lu': 0, 'll': 0, 'lt': 0, 'lm': 0, 'lo': 0, 'mn': 0, 'mc': 0, 'me': 0, 'nd': 0, 'nl': 0, 'no': 0,
		'pc': 0, 'pd': 0, 'ps': 0, 'pe': 0, 'pi': 0, 'pf': 0, 'po': 0, 'sm': 0, 'sc': 0, 'sk': 0, 'so': 0,
		'zs': 1, 'zl': 2, 'zp': 2, 'cc': 2, 'cf': 2, 'cs': 2, 'co': 2, 'cn': 2
	})

	# prepare the control state (C0 or C1 in General_Category https://www.unicode.org/reports/tr44/#GC_Values_Table)
	controlRanges = RemapRanges(categoryRanges, categoryMap, {
		'lu': 0, 'll': 0, 'lt': 0, 'lm': 0, 'lo': 0, 'mn': 0, 'mc': 0, 'me': 0, 'nd': 0, 'nl': 0, 'no': 0,
		'pc': 0, 'pd': 0, 'ps': 0, 'pe': 0, 'pi': 0, 'pf': 0, 'po': 0, 'sm': 0, 'sc': 0, 'sk': 0, 'so': 0,
		'zs': 0, 'zl': 0, 'zp': 0, 'cc': 1, 'cf': 0, 'cs': 0, 'co': 0, 'cn': 0
	})

	# write the state to the file
	with open('test-unicode.h', 'w', encoding='ascii') as file:
		BeginFile(file)
		
		# write the ascii-test to the file
		WriteBinaryLookup(file, asciiRanges, 0, 'TestAscii', 'Ascii', 'detail')
		file.write('\n')

		# write the alpha-test to the file
		WriteBinaryLookup(file, alphaRanges, 0, 'TestAlpha', 'Alpha', 'detail')
		file.write('\n')

		# write the digit-test to the file
		WriteBinaryLookup(file, digitRanges, 0, 'TestDigit', 'Digit', 'detail')
		file.write('\n')

		# write the whitespace-test to the file
		WriteBinaryLookup(file, whiteSpaceRanges, 0, 'TestWhiteSpace', 'WhiteSpace', 'detail')
		file.write('\n')
		
		# write the printable-enum to the file
		printableEnumName, printableEnumMap = 'PrintableType', {
			'printable': 0, 'printSpace': 1, 'none': 2
		}
		WriteEnumString(file, printableEnumName, printableEnumMap)
		WriteEnumLookup(file, printableRanges, 'none', f'detail::{printableEnumName}', printableEnumMap, 'TestPrintable', 'Printable', 'detail')
		file.write('\n')
		
		# write the control-test to the file
		WriteBinaryLookup(file, controlRanges, 0, 'TestControl', 'Control', 'detail')
		
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
		WriteEnumLookup(file, ranges, 'other', f'detail::{graphemeEnumName}', graphemeEnumMap, 'GraphemeType', 'Grapheme', 'detail')
		
		EndFile(file)




# MakeCodepointTesting()
MakeGraphemeTypeMapping()
