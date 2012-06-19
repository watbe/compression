compression
===========

A course project which involved coding several common compression algorithms in Haskell.

Compile using <pre>ghc --make -O2 Main.hs -o compressor</pre>

Compress the file halting using Huffman coding
This produces the file CalgaryCorpus/halting.huff

<pre>./compressor Huffman CalgaryCorpus/halting</pre>

Decompress the file we just encoded.
Produces the file CalgaryCorpus/halting.huff.decoded
This will decompress all algorithms implemented in this assignment

<pre>./compressor Decompress CalgaryCorpus/halting.huff</pre>

Check that there are no differences between the original and decoded files
If nothing is printed, then the files are the same

<pre>diff CalgaryCorpus/halting CalgaryCorpus/halting.huff.decoded</pre>