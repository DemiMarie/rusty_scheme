typedef struct { size_t contents; } Value;

typedef struct {
  Value *start;
  size_t length;
} Heap;

void mark_and_collect(Heap *stack, Heap *fromspace, Heap *tospace) {
  Value *end = tospace->start;

  for (Value *current = stack->start;
       ((size_t)current) - ((size_t)stack) < stack->length;) {
    copy(&current, &end);
  }

  for (Value *current = tospace->start; (size_t)current < ((size_t)end);) {
    copy(&current, &end);
  }
}
