ghc -fforce-recomp -o bin\ListFiles.exe --make ListFiles -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\ListTokens.exe --make ListTokens -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\CreateTree.exe --make  CreateTree -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\ID3.exe --make  ID3 -outputdir bin -Wall -Werror

hlint .
