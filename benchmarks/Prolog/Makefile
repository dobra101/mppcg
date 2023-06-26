.SUFFIXES:

.PHONY: all clean

all: Train1_Lukas_POR Train_1_beebook_deterministic_MC
PROBCLI=./probcli.sh
FLAGS=--model-check --timeout 3600000 -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory
OUTPUT ?= runtimes.txt

% :
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
