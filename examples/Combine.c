// This file is automatically generated.

#include <avr/io.h>
#include <stdbool.h>

static void clock();

static void stream_1(unsigned int input_0);

static void stream_10(bool input_0);

static void stream_2();

static void stream_3(bool input_0);

static void stream_4();

static void stream_5(int arg, void* value);

static void stream_6(bool input_0);

static void stream_7();

static void stream_8(bool input_0);

static void stream_9(int arg, void* value);

static void clock() {
  unsigned int output;
  static unsigned int temp0 = 0U;
  temp0++;
  output = temp0;
  stream_1(output);
}

static void stream_1(unsigned int input_0) {
  bool output;
  output = (input_0) % 2 == 0;
  stream_5(1, (void*)(&output));
  stream_8(output);
}

static void stream_10(bool input_0) {
  bool output;
  if (input_0) {
    PORTB |= 0x04U;
  } else {
    PORTB &= ~(0x04U);
  }
}

static void stream_2() {
  bool output;
  output = (PINB & 0x10U) == 0x10U;
  stream_3(output);
}

static void stream_3(bool input_0) {
  bool output;
  if (input_0) {
    PORTB |= 0x20U;
  } else {
    PORTB &= ~(0x20U);
  }
}

static void stream_4() {
  bool output;
  output = (PINB & 0x10U) == 0x10U;
  stream_5(0, (void*)(&output));
}

static void stream_5(int arg, void* value) {
  bool output;
  static bool input_0;
  static bool input_1;
  switch (arg) {
    case 0:
      input_0 = *((bool*)value);
      break;
    case 1:
      input_1 = *((bool*)value);
      break;
  }
  if (input_0) {
    output = input_1;
  } else {
    output = false;
  }
  output = output;
  stream_6(output);
}

static void stream_6(bool input_0) {
  bool output;
  if (input_0) {
    PORTB |= 0x08U;
  } else {
    PORTB &= ~(0x08U);
  }
}

static void stream_7() {
  bool output;
  output = (PINB & 0x10U) == 0x10U;
  stream_9(0, (void*)(&output));
}

static void stream_8(bool input_0) {
  bool output;
  output = !(input_0);
  stream_9(1, (void*)(&output));
}

static void stream_9(int arg, void* value) {
  bool output;
  static bool input_0;
  static bool input_1;
  switch (arg) {
    case 0:
      input_0 = *((bool*)value);
      break;
    case 1:
      input_1 = *((bool*)value);
      break;
  }
  if (input_0) {
    output = input_1;
  } else {
    output = false;
  }
  output = output;
  stream_10(output);
}

int main(void) {
  TCCR1B = (1 << CS12) | (1 << CS10);
  DDRB |= 0x04U;
  DDRB &= ~(0x10U);
  PORTB |= 0x10U;
  DDRB |= 0x20U;
  DDRB &= ~(0x10U);
  PORTB |= 0x10U;
  DDRB |= 0x08U;
  DDRB &= ~(0x10U);
  PORTB |= 0x10U;
  while (1) {
    if (TCNT1 >= 10000) {
      TCNT1 = 0;
      clock();
    }
    stream_2();
    stream_4();
    stream_7();
  }
  return 0;
}