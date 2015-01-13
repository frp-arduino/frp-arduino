// This file is automatically generated.

#include <avr/io.h>
#include <stdbool.h>

static void input_pin12();

static void input_timer();

static void stream_1(int input_0);

static void stream_10(bool input_0);

static void stream_2(int input_0);

static void stream_3(int input_0);

static void stream_4(int input_0);

static void stream_5(bool input_0);

static void stream_6(int arg, void* value);

static void stream_7(bool input_0);

static void stream_8(bool input_0);

static void stream_9(int arg, void* value);

static void input_pin12() {
  bool temp0;
  temp0 = (PINB & (1 << PB4)) == 0U;
  stream_5(temp0);
  stream_6(0, (void*)(&temp0));
  stream_9(0, (void*)(&temp0));
}

static void input_timer() {
  int temp1;
  temp1 = TCNT1;
  TCNT1 = 0;
  stream_1(temp1);
}

static void stream_1(int input_0) {
  static int fold_state = 0;
  int temp2;
  bool temp3;
  temp3 = fold_state > 10000;
  int temp4;
  temp4 = input_0 + fold_state;
  int temp5;
  temp5 = temp4 - 10000;
  int temp6;
  temp6 = input_0 + fold_state;
  if (temp3) {
    temp2 = temp5;
  } else {
    temp2 = temp6;
  }
  fold_state = temp2;
  stream_2(fold_state);
}

static void stream_10(bool input_0) {
  if (input_0) {
    PORTB |= (1 << PB2);
  } else {
    PORTB &= ~(1 << PB2);
  }
}

static void stream_2(int input_0) {
  bool temp7;
  temp7 = input_0 > 10000;
  bool temp8;
  temp8 = false;
  if (temp7) {
    temp8 = true;
  }
  if (temp8) {
    stream_3(input_0);
  }
}

static void stream_3(int input_0) {
  static int fold_state = 0;
  int temp9;
  temp9 = fold_state + 1;
  fold_state = temp9;
  stream_4(fold_state);
}

static void stream_4(int input_0) {
  bool temp10;
  temp10 = (input_0) % 2 == 0;
  stream_6(1, (void*)(&temp10));
  stream_8(temp10);
}

static void stream_5(bool input_0) {
  if (input_0) {
    PORTB |= (1 << PB5);
  } else {
    PORTB &= ~(1 << PB5);
  }
}

static void stream_6(int arg, void* value) {
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
  bool temp11;
  bool temp12;
  temp12 = false;
  if (input_0) {
    temp11 = input_1;
  } else {
    temp11 = temp12;
  }
  stream_7(temp11);
}

static void stream_7(bool input_0) {
  if (input_0) {
    PORTB |= (1 << PB3);
  } else {
    PORTB &= ~(1 << PB3);
  }
}

static void stream_8(bool input_0) {
  bool temp13;
  temp13 = !(input_0);
  stream_9(1, (void*)(&temp13));
}

static void stream_9(int arg, void* value) {
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
  bool temp14;
  bool temp15;
  temp15 = false;
  if (input_0) {
    temp14 = input_1;
  } else {
    temp14 = temp15;
  }
  stream_10(temp14);
}

int main(void) {
  DDRB &= ~(1 << PB4);
  PORTB |= (1 << PB4);
  TCCR1B |= (1 << CS12);
  TCCR1B |= (1 << CS10);
  DDRB |= (1 << PB2);
  DDRB |= (1 << PB5);
  DDRB |= (1 << PB3);
  while (1) {
    input_pin12();
    input_timer();
  }
  return 0;
}
