"""Entry point for launching the PeTTa Jupyter kernel"""

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    from .kernel import PeTTaKernel
    IPKernelApp.launch_instance(kernel_class=PeTTaKernel)
