#include <stdbool.h>
#include <avr/io.h>
#include <util/delay.h>
#include "copilot_cords.h"

#define RCLK PB2

#define SEG_A (1 << 0)
#define SEG_B (1 << 1)
#define SEG_C (1 << 2)
#define SEG_D (1 << 3)
#define SEG_E (1 << 4)
#define SEG_F (1 << 5)
#define SEG_G (1 << 6)
#define SEG_DP (1 << 7)

const uint8_t digits_ca[10] = {
    ~(SEG_A | SEG_B | SEG_C | SEG_D | SEG_E | SEG_F),         // 0
    ~(SEG_B | SEG_C),                                         // 1
    ~(SEG_A | SEG_B | SEG_G | SEG_E | SEG_D),                 // 2
    ~(SEG_A | SEG_B | SEG_C | SEG_D | SEG_G),                 // 3
    ~(SEG_F | SEG_G | SEG_B | SEG_C),                         // 4
    ~(SEG_A | SEG_F | SEG_G | SEG_C | SEG_D),                 // 5
    ~(SEG_A | SEG_F | SEG_G | SEG_E | SEG_C | SEG_D),         // 6
    ~(SEG_A | SEG_B | SEG_C),                                 // 7
    ~(SEG_A | SEG_B | SEG_C | SEG_D | SEG_E | SEG_F | SEG_G), // 8
    ~(SEG_A | SEG_B | SEG_C | SEG_D | SEG_F | SEG_G)          // 9
};

void spi_init(void) {
    // MOSI and SCK as output, others as input
    DDRB |= (1 << RCLK) | (1 << PB3) | (1 << PB5);
    PORTB &= ~(1 << RCLK);  // RCLK low initially

    DDRD |= (1 << PB6) | (1 << PB7);

    // Enable SPI, Master mode, Fosc/16
    SPCR = (1 << SPE) | (1 << MSTR) | (1 << SPR0);
}

void show_7seg(uint8_t digit, uint8_t data) {
    change_digit(digit);
    SPDR = digits_ca[data % 10];
    while (!(SPSR & (1 << SPIF)));       // Wait for completion
    latch();
}

void latch(void) {
    PORTB |= (1 << RCLK);
    _delay_us(1);
    PORTB &= ~(1 << RCLK);
}

void change_digit(uint8_t x){
    uint8_t mask = (1 << PB6) | (1 << PB7);
    uint8_t pbs[2] = {PB7, PB6};
    PORTD = (PORTD & !mask) | (1 << pbs[x]);
}

bool button_plus = false;
bool button_minus = false;
bool button_A = false;
bool button_B = false;

void update_inputs(){
    button_plus = !(PINC & (1 << PC0));
    button_minus = !(PINC & (1 << PC1));
    button_A = !(PINC & (1 << PC3));
    button_B = !(PINC & (1 << PC2));
}

void change_led(bool x){
    if (x) {
        PORTB |= (1 << PB0);
    } else {
        PORTB &= ~(1 << PB0);
    }
}

int main(void) {
    // Set PB0 as output
    spi_init();

    DDRB |= (1 << PB0); // For LED.

    DDRC &= ~((1 << PC0) | (1 << PC1) | (1 << PC2) | (1 << PC3));  // Buttons.
    PORTC |=  (1 << PC0) | (1 << PC1) | (1 << PC2) | (1 << PC3);
    
    change_digit(0);

    while (1) {
        _delay_ms(1);
        update_inputs();
        step();
    }
}

