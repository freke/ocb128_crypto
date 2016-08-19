#include <stdio.h>
#include <string.h>

#include "crypt.h"

#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; if (message) return message; } while (0)

int tests_run = 0;

int foo = 7;
int bar = 4;

int hex2data(unsigned char *data, const unsigned char *hexstring, unsigned int len)
{
    unsigned const char *pos = hexstring;
    char *endptr;
    size_t count = 0;

    if ((hexstring[0] == '\0') || (strlen(hexstring) % 2)) {
        //hexstring contains no data
        //or hexstring has an odd length
        return -1;
    }

    for(count = 0; count < len; count++) {
        char buf[3] = {pos[0], pos[1], 0};
        data[count] = strtol(buf, &endptr, 16);
        pos += 2 * sizeof(char);

        if (endptr[0] != '\0') {
            //non-hexadecimal character encountered
            return -1;
        }
    }

    return 0;
}

static char * test_foo() {
    mu_assert("error, foo != 7", foo == 7);
    return 0;
}

static char * test_bar() {
    mu_assert("error, bar != 5", bar == 5);
    return 0;
}

static char * test_OCB_AES_128_3B() {
    const unsigned char hex_key_string[] = "000102030405060708090a0b0c0d0e0f";
    const unsigned char hex_nonce_string[] = "00000000000000000000000000000001";
    const unsigned char hex_plain_string[] = "000102";

    unsigned char key_hex[16], nonce_hex[16], tag_hex[16], plain_hex[3];
    unsigned char tag_encrypt[16], tag_decrypted[16], nonce[16], cipher[3], decrypted[3];
    cryptState_t cs;
    size_t count = 0;

    if(hex2data(key_hex, hex_key_string, 16)) {
        return "error key";
    }

    if(hex2data(nonce_hex, hex_nonce_string, 16)){
        return "error nonce";
    }

    if(hex2data(plain_hex, hex_plain_string, 3)) {
        return "error plain";
    }

    CryptState_init(&cs);
    CryptState_setKey(&cs,key_hex,key_hex,key_hex);
    CryptState_ocb_encrypt(&cs, plain_hex, cipher, 3, nonce_hex, tag_encrypt);
    CryptState_ocb_decrypt(&cs, cipher, decrypted, 3, nonce_hex, tag_decrypted);

    for (count = 0; count < 16; count++){
        mu_assert("error, tag", tag_encrypt[count] == tag_decrypted[count]);
    }

    for (count = 0; count < 3; count++){
        mu_assert("error, decrypt", plain_hex[count] == decrypted[count]);
    }

    return 0;
}

/*
Test case  OCB-AES-128-3B
Key        000102030405060708090a0b0c0d0e0f
Nonce      00000000000000000000000000000001
Plaintext  000102
Ciphertext fcd37d
Tag        02254739a5e3565ae2dcd62c659746ba

Test case  OCB-AES-128-16B
Key        000102030405060708090a0b0c0d0e0f
Nonce      00000000000000000000000000000001
Plaintext  000102030405060708090a0b0c0d0e0f
Ciphertext 37df8ce15b489bf31d0fc44da1faf6d6
Tag        dfb763ebdb5f0e719c7b4161808004df

Test case  OCB-AES-128-20B
Key        000102030405060708090a0b0c0d0e0f
Nonce      00000000000000000000000000000001
Plaintext  000102030405060708090a0b0c0d0e0f10111213
Ciphertext 01a075f0d815b1a4e9c881a1bcffc3eb7003eb55
Tag        753084144eb63b770b063c2e23cda0bb

Test case  OCB-AES-128-32B
Key        000102030405060708090a0b0c0d0e0f
Nonce      00000000000000000000000000000001
Plaintext  000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
Ciphertext 01a075f0d815b1a4e9c881a1bcffc3eb4afcbb7fedc08ca8654c6d304d1612fa
Tag        c14cbf2c1a1f1c3c137eadea1f2f2fcf

Test case  OCB-AES-128-34B
Key        000102030405060708090a0b0c0d0e0f
Nonce      00000000000000000000000000000001
Plaintext  000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021
Ciphertext 01a075f0d815b1a4e9c881a1bcffc3ebd4903dd0025ba4aa837c74f121b0260fa95d
Tag        cf8341bb10820ccf14bdec56b8d7d6ab
*/

static char * all_tests() {
    mu_run_test(test_foo);
    mu_run_test(test_OCB_AES_128_3B);
    return 0;
}

int main(int argc, char **argv) {
    char *result = all_tests();
    if (result != 0) {
        printf("%s\n", result);
    }
    else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tests run: %d\n", tests_run);

    fflush(NULL);

    return result != 0;
}
