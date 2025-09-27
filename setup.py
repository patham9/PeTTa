from setuptools import setup, find_packages

setup(
    name="petta",
    version="0.1.0",
    packages=find_packages(where="python"),
    package_dir={"": "python"},
    include_package_data=True,
    install_requires=[
        'janus',
    ],
    author="Your Name",
    author_email="your.email@example.com",
    description="A Python wrapper for MeTTa",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/your-repo-url",  # Replace with actual repo URL
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",  # Adjust based on LICENSE
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.6",
)
