MCU=atmega328p
F_CPU=10000000UL
CC=avr-gcc
OBJCOPY=avr-objcopy
CFLAGS=-Wall -Os -mmcu=$(MCU) -DF_CPU=$(F_CPU)\
-fdata-sections -ffunction-sections -Wl,--gc-sections
PROGRAMMER=avrispmkII
TARGET = main
SRC = main.c copilot_cords.c

HEX = $(TARGET).hex
ELF = $(TARGET).elf

all: $(HEX)

$(ELF): $(SRC)
	$(CC) $(CFLAGS) $^ -o $@

$(HEX): $(ELF)
	$(OBJCOPY) -O ihex -R .eeprom $< $@

flash: $(HEX)
	avrdude -c $(PROGRAMMER) -p $(MCU) -U flash:w:main.hex

clean:
	rm -f *.elf *.hex *.o

