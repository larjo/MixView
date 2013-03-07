del /Q bin\*

ghc -o bin/ListFiles.exe ListFiles -outputdir bin -Wall -Werror
ghc -o bin/ListTokens.exe ListTokens -outputdir bin -Wall -Werror
ghc -o bin/Main.exe Main -outputdir bin -Wall -Werror

del /Q bin\*.o
del /Q bin\*.hi

hlint .
