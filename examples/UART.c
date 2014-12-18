#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>

static void clock();

static void stream_1(bool input_0);

static void stream_2(unsigned int input_0);

static void stream_3(char * input_0);

static void stream_4(unsigned int input_0);

static void clock() {
  unsigned int output;
  static unsigned int temp0 = 0U;
  temp0++;
  output = temp0;
  stream_2(output);
  stream_4(output);
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

static void stream_3(char * input_0) {
  char * output;
  while (*input_0 != 0) {
    while ((UCSR0A & (1 << UDRE0)) == 0) {
    }
    UDR0 = *input_0;
    input_0++;
  }
}

static void stream_4(unsigned int input_0) {
  char * output;
  char temp1[] = "hello\r\n";
  output = temp1;
  stream_3(output);
}

int main(void) {
  DDRB |= 0x20U;
  #define F_CPU 16000000UL
  #define BAUD 9600
  #include <util/setbaud.h>
  UBRR0H = UBRRH_VALUE;
  UBRR0L = UBRRL_VALUE;
  #if USE_2X
    UCSR0A |= (1 << U2X0);
  #else
    UCSR0A &= ~((1 << U2X0));
  #endif
  UCSR0C = (1 << UCSZ01) |(1 << UCSZ00);
  UCSR0B = (1 << RXEN0) | (1 << TXEN0);
  while (1) {
    clock(0);
    _delay_ms(1000);
  }
  return 0;
}
