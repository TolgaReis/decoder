# Decoder
&nbsp;&nbsp;&nbsp;&nbsp;A document encoded with a cipher alphabet. I am asked to implement two different methods to break the cipher,
1. a brute force version that uses a spell checker, and
2. a version that uses knowledge about the distribution of particular letters in the English
language (frequency analysis).

&nbsp;&nbsp;&nbsp;&nbsp;Need to write two functions Gen-Decoder-A and Gen-Decoder-B that take as input a paragraph of encoded words, and return as output a function that takes as input an encoded word, and returns the plain text of the decoded word. This function can then be used to decode the entire document which may consist of multiple paragraphs. The function Code-Breaker takes an encoded document and a decoding function as input, and returns the entire document in plain text. The two decoder functions implement different strategies to break the encrypted code. Both have advantages and disadvantages.

