#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>

static void clock();

static void stream_1(bool input_0);

static void stream_2(unsigned int input_0);

static void clock() {
  unsigned int output;
  static unsigned int temp0 = 0U;
  temp0++;
  output = temp0;
  stream_2(output);
}

static void stream_1(bool input_0) {
  bool output;
  if (input_0) {
    PORTB |= 0x20U;
  } else {
    PORTB &= ~(0x20U);
  }
}

static void stream_2(unsigned int input_0) {
  bool output;
  output = (input_0) % 2 == 0;
  stream_1(output);
}

int main(void) {
  DDRB |= 0x20U;
  while (1) {
    clock(0);
    _delay_ms(1000);
  }
  return 0;
}
