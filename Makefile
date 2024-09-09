build:
	dotnet fable -o ./build

watch:
	dotnet fable watch -o ./build

clean:
	rm -rf ./build

.PHONY: build watch clean
