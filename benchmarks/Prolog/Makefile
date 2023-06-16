.SUFFIXES:

.PHONY: all clean

all: Lift
PROBCLI=./probcli.sh
FLAGS=--model-check -disable-time-out -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory
OUTPUT ?= runtimes.txt

% :
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)

clean:
	rm *.prob
