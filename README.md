Mixmeister Playlist Viewer
==========================

This is a simple program that parses Mixmeister playlists, which is a type of RIFF-file, into a stream of tokens.

There are two structures that the RIFF-file can be parsed into:
A flat structure, equvalent to RiffChunks = List(Data|List)*:

```haskell
data Chunk = DataChunk Data
           | ListChunk List

data RiffChunks = RiffChunks List [Chunk]
```


and a tree structure:

```haskell
data Tree = DataNode Data
          | ListNode Riff

data Riff = Riff Format [Tree]
```
