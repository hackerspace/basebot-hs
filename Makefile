CC=ghc

all: bot.hs
	$(CC) --make bot.hs -o bot
clean:
	rm *.o *.hi 
	
