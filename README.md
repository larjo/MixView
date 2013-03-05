Mixmeister Playlist Viewer
==========================

This is a simple program that parses Mixmeister playlists, which is a type of RIFF-file, into a stream of tokens.

There are two structures that the RIFF-file can be parsed into. A flat structure:
```haskell
data Token = Data Id Len RawData
           | List Id Len Format
```

and a tree structure:
```haskell
data Chunk = List Id Format [Chunk]
           | Data Id RawData
```
