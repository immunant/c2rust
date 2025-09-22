// vm_x86.c -- load time compiler and execution environment for x86 (from ioq3)

/*
  eax		scratch
  ebx/bl	opStack offset
  ecx		scratch (required for shifts)
  edx		scratch (required for divisions)
  esi		program stack
  edi   	opStack base
x86_64:
  r8		vm->instructionPointers
  r9		vm->dataBase
*/

typedef struct {
	int programStack;
	int entryOfs;
	void* dataBase;
	void* codeBase;
	unsigned long* instructionPointers;
} vm_t;

typedef unsigned char byte;

#define OPSTACK_SIZE 256
#define MAX_VMMAIN_ARGS 50

int VM_CallCompiled(vm_t *vm, int *args)
{
	byte	stack[OPSTACK_SIZE + 15];
	void	*entryPoint;
	int		programStack, stackOnEntry;
	byte	*image;
	int	*opStack;
	int		opStackOfs;
	int		arg;

	vm_t* currentVM = vm;

	// we might be called recursively, so this might not be the very top
	programStack = stackOnEntry = vm->programStack;

	// set up the stack frame 
	image = vm->dataBase;

	programStack -= ( 8 + 4 * MAX_VMMAIN_ARGS );

	for ( arg = 0; arg < MAX_VMMAIN_ARGS; arg++ )
		*(int *)&image[ programStack + 8 + arg * 4 ] = args[ arg ];

	*(int *)&image[ programStack + 4 ] = 0;	// return stack
	*(int *)&image[ programStack ] = -1;	// will terminate the loop on return

	// off we go into generated code...
	entryPoint = vm->codeBase + vm->entryOfs;
	opStack = (int*)stack + 16;
	*opStack = 0;
	opStackOfs = 0;

#if __x86_64__
	__asm__ volatile(
		"movq %5, %%rax\n"
		"movq %3, %%r8\n"
		"movq %4, %%r9\n"
		"push %%r15\n"
		"push %%r14\n"
		"push %%r13\n"
		"push %%r12\n"
		"callq *%%rax\n"
		"pop %%r12\n"
		"pop %%r13\n"
		"pop %%r14\n"
		"pop %%r15\n"
		: "+S" (programStack), "+D" (opStack), "+b" (opStackOfs)
		: "g" (vm->instructionPointers), "g" (vm->dataBase), "g" (entryPoint)
		: "cc", "memory", "%rax", "%rcx", "%rdx", "%r8", "%r9", "%r10", "%r11"
	);
#elif __i386__
	__asm__ volatile(
		"calll *%3\n"
		: "+S" (programStack), "+D" (opStack), "+b" (opStackOfs)
		: "g" (entryPoint)
		: "cc", "memory", "%eax", "%ecx", "%edx"
	);
#endif

	if(opStackOfs != 1 || *opStack != 0xDEADBEEF)
	{
		return 0; // opStack corrupted in compiled code
	}
	if(programStack != stackOnEntry - (8 + 4 * MAX_VMMAIN_ARGS))
		return 0; // programStack corrupted in compiled code

	vm->programStack = stackOnEntry;

	return opStack[opStackOfs];
}
