.SUFFIXES:

.PHONY: all clean

all: nota_v2_2
PROBCLI=./probcli.sh
FLAGS=--model-check --timeout 3600000 -p OPERATION_REUSE full -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory
OUTPUT ?= runtimes_operationReuse.txt

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
