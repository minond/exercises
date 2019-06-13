printing
printOn: aStream
  super printOn: aStream.
  aStream nextPutAll: ' with a value of: ', count printString ; cr