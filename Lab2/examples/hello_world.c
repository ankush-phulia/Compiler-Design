void printf(int x, int y, int c), scanf(void);
int a[4], b[10];
float f1, *f2, f2;

int main(void)
{
  int a, b, c;
  printf(a, b, c);

  return 0;
}

void printf(int a, int b, int c)
{
	c = 1;
	if (c+1 == 2 || c - 1 == 0)
	{
		c = 0;
	}
	
	if (!c)
	{
		c = 1;
	}
	else
	{
		c = 0;
	}

	while (!c)
	{
		c = 1;
	}

	do {
		;
	}
	while(!c);

	return;
}

void scanf(void) 
{
	int i;
	for (i = 0; i < 5; i++) {
		return;
	}
}