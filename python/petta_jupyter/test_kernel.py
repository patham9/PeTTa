#!/usr/bin/env python3
"""
Manual test script for PeTTa Jupyter kernel.

This script tests basic functionality without requiring a full Jupyter notebook.
"""

import sys
import tempfile
import os

# Add parent directory to path to find petta module
kernel_dir = os.path.dirname(os.path.abspath(__file__))
python_dir = os.path.dirname(kernel_dir)
sys.path.insert(0, python_dir)

from petta_jupyter.kernel import PeTTaKernel
from petta_jupyter.output_formatter import format_results

class MockSocket:
    """Mock socket for testing"""
    def __init__(self):
        self.messages = []
        self.session = self

    def send(self, msg_type, content=None, parent=None, ident=None, buffers=None, track=False, header=None, metadata=None):
        if content is None:
            content = msg_type
        self.messages.append(content)

class TestKernel:
    def __init__(self):
        self.kernel = PeTTaKernel()
        self.kernel.iopub_socket = MockSocket()
        self.execution_count = 0
        self.tests_passed = 0
        self.tests_failed = 0

    def test(self, name, code, expected_status='ok', should_have_output=None, error_expected=False):
        """Run a test case"""
        print(f"\n{'='*60}")
        print(f"Test: {name}")
        print(f"Code: {code}")
        print(f"{'='*60}")

        self.execution_count += 1
        self.kernel.execution_count = self.execution_count

        # Clear previous messages
        self.kernel.iopub_socket.messages = []

        # Execute code
        result = self.kernel.do_execute(code, silent=False)

        # Check status
        status_ok = result['status'] == expected_status
        if not status_ok:
            print(f"❌ FAIL: Expected status '{expected_status}', got '{result['status']}'")
            self.tests_failed += 1
            return False

        # Check for output if specified
        if should_have_output is not None:
            has_output = len(self.kernel.iopub_socket.messages) > 0
            if has_output != should_have_output:
                print(f"❌ FAIL: Expected output={should_have_output}, got {has_output}")
                self.tests_failed += 1
                return False

        # Display messages
        if self.kernel.iopub_socket.messages:
            print("\nOutput:")
            for content in self.kernel.iopub_socket.messages:
                stream_name = content.get('name', 'unknown')
                text = content.get('text', '')
                marker = "🔴" if stream_name == 'stderr' else "⚪"
                print(f"{marker} [{stream_name}] {text}")
        else:
            print("\nOutput: (none)")

        print(f"\n✅ PASS")
        self.tests_passed += 1
        return True

    def run_all_tests(self):
        """Run comprehensive test suite"""
        print("="*60)
        print("PeTTa Jupyter Kernel Test Suite")
        print("="*60)

        # Test 1: Basic arithmetic
        self.test(
            "Basic arithmetic",
            "!(+ 1 2)",
            should_have_output=True
        )

        # Test 2: Function definition (no output expected)
        self.test(
            "Function definition",
            "(= (double $x) (* $x 2))",
            should_have_output=False
        )

        # Test 3: Use defined function
        self.test(
            "Use defined function",
            "!(double 5)",
            should_have_output=True
        )

        # Test 4: Empty code
        self.test(
            "Empty code",
            "",
            should_have_output=False
        )

        # Test 5: Syntax error
        self.test(
            "Syntax error",
            "!(+ 1",
            should_have_output=True,
            error_expected=True
        )

        # Test 6: Type error
        self.test(
            "Type error",
            "!(+ abc def)",
            should_have_output=True,
            error_expected=True
        )

        # Test 7: Multiple arithmetic operations
        self.test(
            "Multiplication",
            "!(* 3 4)",
            should_have_output=True
        )

        # Test 8: State persistence - use previously defined function
        self.test(
            "State persistence",
            "!(double 10)",
            should_have_output=True
        )

        # Summary
        print("\n" + "="*60)
        print("TEST SUMMARY")
        print("="*60)
        print(f"✅ Passed: {self.tests_passed}")
        print(f"❌ Failed: {self.tests_failed}")
        print(f"Total: {self.tests_passed + self.tests_failed}")
        print("="*60)

        return self.tests_failed == 0

def test_output_formatter():
    """Test output formatter directly"""
    print("\n" + "="*60)
    print("Testing Output Formatter")
    print("="*60)

    tests = [
        # (input, expected_output, expected_is_error)
        ([], (None, False)),
        (['3'], ('3', False)),
        (['10'], ('10', False)),
        (['1', '2', '3'], ('1\n2\n3', False)),
        (["ERROR: error(syntax_error(Parse error),_)"], ("Parse error", True)),
        (["ERROR: error(type_error(evaluable,def/0),context(system:(is)/2,_))"],
         ("Type error: Expected evaluable, got def/0", True)),
    ]

    passed = 0
    failed = 0

    for i, (input_val, expected) in enumerate(tests, 1):
        result = format_results(input_val)
        if result == expected:
            print(f"✅ Test {i}: PASS")
            passed += 1
        else:
            print(f"❌ Test {i}: FAIL")
            print(f"   Input: {input_val}")
            print(f"   Expected: {expected}")
            print(f"   Got: {result}")
            failed += 1

    print(f"\nFormatter Tests: {passed} passed, {failed} failed")
    return failed == 0

if __name__ == "__main__":
    formatter_ok = test_output_formatter()

    tester = TestKernel()
    kernel_ok = tester.run_all_tests()

    if formatter_ok and kernel_ok:
        print("\n🎉 All tests passed!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed")
        sys.exit(1)
