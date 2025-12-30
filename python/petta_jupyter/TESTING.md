# Testing Checklist for PeTTa Jupyter Kernel

Follow these steps to thoroughly test the kernel:

## 1. Verify Installation

```bash
python3 -m jupyter kernelspec list | grep petta
```

**Expected:** Should show `petta` kernel installed in your Jupyter kernels directory.

## 2. Test Kernel Import

```bash
cd /opt/snet/PeTTa/python
python3 -c "from petta_jupyter.kernel import PeTTaKernel; print('✓ Kernel imports successfully')"
```

**Expected:** Should print success message without errors.

## 3. Launch JupyterLab

```bash
python3 -m jupyter lab
```

**Expected:** JupyterLab opens in your browser.

## 4. Create Test Notebook

1. In JupyterLab Launcher (the main tab that opens), look for "PeTTa (MeTTa)" under the "Notebook" section
2. Click on the "PeTTa (MeTTa)" tile

**Expected:** New notebook opens with "PeTTa (MeTTa)" shown in top-right corner.

> **Note:** If you don't see the Launcher, click the "+" button in the top-left toolbar to open it.

## 5. Test Basic Execution

### Test 5.1: Simple Arithmetic
**Input:**
```metta
!(+ 1 2)
```
**Expected Output:** `3` (in normal text)

### Test 5.2: Multiplication
**Input:**
```metta
!(* 3 4)
```
**Expected Output:** `12`

### Test 5.3: Subtraction
**Input:**
```metta
!(- 10 5)
```
**Expected Output:** `5`

✅ **Pass Criteria:** All three operations return correct results in normal (black) text.

## 6. Test Function Definitions

### Test 6.1: Define Function
**Input:**
```metta
(= (double $x) (* $x 2))
```
**Expected Output:** (no output)

### Test 6.2: Use Function
**Input:**
```metta
!(double 5)
```
**Expected Output:** `10`

✅ **Pass Criteria:** Function definition shows no output, function usage returns correct result.

## 7. Test Multiple Results

### Test 7.1: Define Multiple Clauses

**Input:**
```metta
(= (sign -1) negative)
(= (sign 0) zero)
(= (sign 1) positive)
```
**Expected Output:** (no output)

### Test 7.2: Test Different Cases

**Input:**
```metta
!(sign -1)
```
**Expected Output:** `negative`

**Input:**
```metta
!(sign 0)
```
**Expected Output:** `zero`

**Input:**
```metta
!(sign 1)
```
**Expected Output:** `positive`

✅ **Pass Criteria:** Pattern matching works correctly for different values.

> **Known Limitation:** Recursive functions (factorial, fibonacci, etc.) currently cause stack overflow in PeTTa due to clause ordering issues. The general recursive clause continues to match even when base cases should apply, leading to infinite recursion with negative numbers.

## 8. Test Error Handling

### Test 8.1: Syntax Error
**Input:**
```metta
!(+ 1
```
**Expected Output:** Error message in red:
```
Parse error: missing ')', starting at line 1:
+ 1
```

### Test 8.2: Type Error
**Input:**
```metta
!(+ abc def)
```
**Expected Output:** Error message in red:
```
Type error: Expected evaluable, got def/0
```

✅ **Pass Criteria:** Errors display in red text with readable messages. Kernel does NOT crash.

## 9. Test State Persistence

### Test 9.1: Define Function in Cell 1
**Input (Cell 1):**
```metta
(= (triple $x) (* $x 3))
```

### Test 9.2: Use Function in Cell 2
**Input (Cell 2):**
```metta
!(triple 7)
```
**Expected Output:** `21`

✅ **Pass Criteria:** Function defined in one cell can be used in later cells.

## 10. Test Edge Cases

### Test 10.1: Empty Cell
**Input:** (leave cell empty and execute)
**Expected:** No output, no error

### Test 10.2: Whitespace Only
**Input:** `   ` (just spaces)
**Expected:** No output, no error

### Test 10.3: Undefined Function
**Input:**
```metta
!(undefined-function 42)
```
**Expected Output:** `(undefined-function 42)` (in normal text, not error)
> Note: MeTTa returns unreduced expressions when functions aren't defined.

## 11. Test Kernel Restart

1. In notebook menu: "Kernel → Restart Kernel"
2. Confirm restart
3. Try using a previously defined function (e.g., `!(double 5)`)

**Expected:** Should show error or unreduced expression (state is cleared).

4. Re-define the function and try again

**Expected:** Should work after re-definition.

✅ **Pass Criteria:** Kernel restarts cleanly, state resets.

## 12. Test Example Notebook

1. Open `examples/basic_usage.ipynb`
2. Run all cells: "Run → Run All Cells"

**Expected:** All cells execute successfully with correct outputs.

## Final Checklist

- [ ] Installation verified
- [ ] Kernel imports successfully
- [ ] JupyterLab launches
- [ ] Can create notebook with PeTTa kernel
- [ ] Basic arithmetic works (3 tests)
- [ ] Function definitions work
- [ ] Recursive functions work
- [ ] Error handling works (syntax and type errors)
- [ ] State persists across cells
- [ ] Edge cases handled correctly
- [ ] Kernel restart works
- [ ] Example notebook runs successfully

## Troubleshooting

### Kernel won't start
- Check janus-swi is installed: `pip list | grep janus`
- Check SWI-Prolog is installed: `which swipl`

### No output showing
- Make sure you executed the cell (Shift+Enter)
- Check kernel status in top-right (should show "PeTTa (MeTTa)")

### Kernel crashes
- Look at terminal where you ran `jupyter lab` for error messages
- Try restarting kernel: "Kernel → Restart Kernel"

### Import errors
- Make sure you're in the right directory when launching JupyterLab
- Try reinstalling: `cd /opt/snet/PeTTa/python/petta_jupyter && ./install.sh`

## Success!

If all tests pass, the kernel is working correctly! 🎉
