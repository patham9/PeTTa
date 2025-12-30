"""Setup script for PeTTa Jupyter kernel

Installation:
    pip install -e /path/to/PeTTa/python/petta_jupyter

After installation, register the kernel:
    jupyter kernelspec install --user /path/to/PeTTa/python/petta_jupyter/resources

Or use the provided install.sh script for automatic installation.
"""

from setuptools import setup

setup(
    name='petta-jupyter',
    version='0.1.0',
    description='Jupyter kernel for PeTTa (MeTTa language)',
    long_description=open('README.md').read() if __file__ else '',
    long_description_content_type='text/markdown',
    author='SingularityNET',
    url='https://github.com/patham9/PeTTa',
    py_modules=['__init__', '__main__', 'kernel', 'output_formatter'],
    package_data={
        '': ['resources/kernel.json'],
    },
    include_package_data=True,
    install_requires=[
        'ipykernel>=6.0.0',
        'janus-swi>=1.5.0',
    ],
    python_requires='>=3.8',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Intended Audience :: Science/Research',
        'Topic :: Scientific/Engineering :: Artificial Intelligence',
        'Framework :: Jupyter',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
    ],
)
