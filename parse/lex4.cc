

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

int step (int state, char ch)
{
  if (state == 0) {
    if ((ch >= 48 && ch <= 57)) {
      state = 18;
    } else if ((ch == 39)) {
      state = 13;
    } else if ((ch == 35)) {
      state = 4;
    } else if ((ch == 34)) {
      state = 7;
    } else if ((ch == 10)) {
      state = 3;
    } else if ((ch == 9) || (ch == 32)) {
      state = 2;
    } else if ((ch >= 65 && ch <= 90) || (ch == 95) || (ch >= 97 && ch <= 122)) {
      state = 19;
    } else if ((ch == 37) || (ch >= 40 && ch <= 47) || (ch == 58) || (ch >= 60 && ch <= 62) || (ch == 91) || (ch == 93) || (ch == 123) || (ch == 125)) {
      state = 12;
    } else {
      state = 1;
    }
  } else if (state == 1) {
    state = 1;
  } else if (state == 2) {
    if ((ch == 35)) {
      state = 4;
    } else if ((ch == 10)) {
      state = 3;
    } else if ((ch == 9) || (ch == 32)) {
      state = 2;
    } else {
      state = 1;
    }
  } else if (state == 3) {
    if ((ch >= 9 && ch <= 10) || (ch == 32)) {
      state = 3;
    } else {
      state = 1;
    }
  } else if (state == 4) {
    if ((ch == 10)) {
      state = 6;
    } else {
      state = 5;
    }
  } else if (state == 5) {
    if ((ch == 10)) {
      state = 6;
    } else {
      state = 5;
    }
  } else if (state == 6) {
    state = 1;
  } else if (state == 7) {
    if ((ch == 92)) {
      state = 10;
    } else if ((ch == 34)) {
      state = 9;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 8;
    }
  } else if (state == 8) {
    if ((ch == 92)) {
      state = 10;
    } else if ((ch == 34)) {
      state = 9;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 8;
    }
  } else if (state == 9) {
    state = 1;
  } else if (state == 10) {
    state = 11;
  } else if (state == 11) {
    if ((ch == 92)) {
      state = 10;
    } else if ((ch == 34)) {
      state = 9;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 8;
    }
  } else if (state == 12) {
    state = 1;
  } else if (state == 13) {
    if ((ch == 92)) {
      state = 16;
    } else if ((ch == 39)) {
      state = 15;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 14;
    }
  } else if (state == 14) {
    if ((ch == 92)) {
      state = 16;
    } else if ((ch == 39)) {
      state = 15;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 14;
    }
  } else if (state == 15) {
    state = 1;
  } else if (state == 16) {
    state = 17;
  } else if (state == 17) {
    if ((ch == 92)) {
      state = 16;
    } else if ((ch == 39)) {
      state = 15;
    } else if ((ch == 10)) {
      state = 1;
    } else {
      state = 14;
    }
  } else if (state == 18) {
    if ((ch >= 48 && ch <= 57)) {
      state = 18;
    } else {
      state = 1;
    }
  } else if (state == 19) {
    if ((ch >= 48 && ch <= 57) || (ch >= 65 && ch <= 90) || (ch == 95) || (ch >= 97 && ch <= 122)) {
      state = 20;
    } else {
      state = 1;
    }
  } else if (state == 20) {
    if ((ch >= 48 && ch <= 57) || (ch >= 65 && ch <= 90) || (ch == 95) || (ch >= 97 && ch <= 122)) {
      state = 20;
    } else {
      state = 1;
    }
  }
  return state;
}

char * finals[] = {
  0,
  0,
  "whitespace",
  "whitespace",
  0,
  0,
  "comment",
  0,
  0,
  "string1",
  0,
  0,
  "punctuation",
  0,
  0,
  "string2",
  0,
  0,
  "number",
  "ident",
  "ident",
};

#include <string>

int main (int argc, char * argv[])
{
  char buffer[16384];
  int fd = open ("lexer.py", O_RDONLY);
  int len = read (fd, buffer, 16384);
  int j=0,i=0,state=0;
  char * last=0;
  char * final=0;
  std::string current;
  
  for (j=0; j < 1000; j++) {
    for (i=0; i < len; i++) {
      //fprintf (stderr, "[%d|%d]", buffer[i], state);
      while (1) {
	state = step (state, buffer[i]);
	final = finals[state];
	if (!last && final) {
	  last = final;
	  break;
	} else if (last && !final) {
	  //fprintf (stderr, ".%s", last);
	  //fprintf (stderr, "<%s>", current.data());
	  state = 0;
	  last = 0;
	  current.erase();
	} else {
	  break;
	}
      }
      current += buffer[i];
    }
  }
}

