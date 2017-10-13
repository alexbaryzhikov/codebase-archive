'''
======================================================

LZW encoder/decoder

Usage:

Encode( 'file' )
Decode( 'file.lzw' )

======================================================
'''

M_EOD = 256		# end-of-data marker
NEW_IDX = 257	# first free dictionary index
MAX_BITS = 15	# maximum index bits

def Encode( inputFileName ):
	'''
	Encodes the input file and writes the result to .lzw file
	'''

	def InitDict():
		'''
		Returns default encoding dictionary
		'''
		res = {}
		b = bytes( range( 256 ) )			# ascii
		for i in range( len( b ) ):
			res[ b[ i : i + 1 ] ] = i
		return res

	def ReadStream():
		'''
		Reads bytes from inFile and yields them one at a time
		Yields 'EOD' upon reaching EOF
		'''
		while True:
			buf = inFile.read( 1024 )		# read buffer
			if len( buf ) == 0: break
			for i in range( len( buf ) ):
				yield buf[ i : i + 1 ]		# yield 1 byte
		yield 'EOD'

	def WriteStream():
		'''
		Takes codes, packs them to bytes and writes to outFile
		'''
		outBuffer = b''
		tmp, tmpBits = 0, 0
		while True:
			code = yield
			tmp = tmp << idxBits | code
			tmpBits += idxBits
			while tmpBits >= 8: 			# move full bytes to buffer
				tmpBits -= 8
				outBuffer += bytes( [ tmp >> tmpBits ] )
				tmp &= ( 1 << tmpBits ) - 1
			if code == M_EOD: 				# dump what's left and return
				if tmpBits:
					outBuffer += bytes( [ tmp << 8 - tmpBits ] )
				outFile.write( outBuffer )
				yield
			if len( outBuffer ) >= 1024: 	# dump buffer to file
				outFile.write( outBuffer )
				outBuffer = b''

	dictionary = InitDict()
	dictFull = False
	inFile = open( inputFileName, 'rb' )
	inStream = ReadStream()
	outFile = open( inputFileName + '.lzw', 'wb' )
	outStream = WriteStream()
	outStream.send( None )
	seq = b''
	idx, idxBits, nextShift = NEW_IDX, 9, 512
	
	while True:
		inByte = next( inStream )		# read the next byte
		if inByte == 'EOD':				# end of data
			if seq: outStream.send( dictionary[ seq ] )
			outStream.send( M_EOD )
			break
		newSeq = seq + inByte	
		if newSeq not in dictionary:
			outStream.send( dictionary[ seq ] ) # emit the dictionary index
			seq = inByte
			if not dictFull:
				dictionary[ newSeq ] = idx 		# add the new sequence to the dictionary
				idx += 1
				if idx == nextShift:
					if idxBits + 1 <= MAX_BITS: idxBits += 1; nextShift <<= 1
					else: dictFull = True
		else: seq = newSeq
				
	print( 'Encoding of ' + inputFileName + ' is finished.' )
	print( 'Compression ratio:', end = ' ' )
	if inFile.tell():
		print( round( 100 * ( 1 - outFile.tell() / inFile.tell() ), 2 ), '%' )
	else:
		print( '0 %' )
	inFile.close()
	outFile.close()


def Decode( inputFileName ):
	'''
	Decodes the input file and writes the result to .out file
	'''

	def InitDict():
		'''
		Returns default decoding dictionary
		'''
		res = {}
		b = bytes( range( 256 ) )			# ascii
		for i in range( len( b ) ):
			res[ i ] = b[ i : i + 1 ]
		return res

	def ReadStream():
		'''
		Reads data from inFile and yields dictionary indexes
		'''
		tmp, tmpBits, i, bufSize = 0, 0, 0, 0
		buf = b''
		while True:
			while tmpBits < idxBits:
				if i == bufSize:			# fill buffer
					buf = inFile.read( 1024 )
					if not buf: yield -1
					bufSize = len( buf )
					i = 0
				tmp = tmp << 8 | buf[ i ]
				tmpBits += 8
				i += 1
			tmpBits -= idxBits
			yield tmp >> tmpBits 			# yield a code
			tmp &= ( 1 << tmpBits ) - 1

	def WriteStream():
		'''
		Takes bytes and writes them to outFile
		'''
		while True:
			buf = b''
			while len( buf ) < 1024:
				tmp = yield
				if tmp: buf += tmp
				else: break
			outFile.write( buf )

	if inputFileName[ -4 : ] != '.lzw':
		print( 'Input file is not an .lzw archive.' )
		return
		
	dictionary = InitDict()
	dictFull = False
	inFile = open( inputFileName, 'rb' )
	inStream = ReadStream()
	outFile = open( inputFileName[ : -4 ] + '.out', 'wb' )
	outStream = WriteStream()
	outStream.send( None )
	seq, newSeq = b'', b''
	idx, idxBits, nextShift = NEW_IDX, 9, 512
	status = 0

	while True:
		code = next( inStream )				# read the next index
		if code == -1: status = -1; break 	# data is corrupted
		if code == M_EOD: status = 1; break # stop code reached
		if code in dictionary:
			seq = dictionary[ code ]
			if not dictFull:
				newSeq += seq[ 0 : 1 ]
				if len( newSeq ) > 1:		# update dictionary
					dictionary[ idx ] = newSeq
					idx += 1
				newSeq = seq
		elif code == idx:					# cScSc case
			if dictFull: status = -2; break # index is out of range
			newSeq += newSeq[ 0 : 1 ]
			dictionary[ idx ] = newSeq		# update dictionary
			idx += 1
			seq = newSeq
		else: status = -2; break			# index is out of range
		outStream.send( seq )				# emit decoded bytes
		if idx + 1 == nextShift:
			nextShift <<= 1
			if idxBits + 1 <= MAX_BITS: idxBits += 1
			else: dictFull = True
		
	outStream.send( None )
	if status == 1:
		print( 'Decoding of ' + inputFileName + ' is finished.')
	elif status == -1:
		print( inputFileName + ' is corrupted.' )
	elif status == -2:
		print( 'Decoding of ' + inputFileName + ' failed: unknown index - ' + str( code ) )
	inFile.close()
	outFile.close()
