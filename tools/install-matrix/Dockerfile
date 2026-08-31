# The local half of the install matrix (task #876): the SAME recipe the
# GitHub container matrix runs (.github/workflows/install-matrix.yml), as a
# parametrized image build so it can run on any machine with docker/podman.
# Driven by tools/install-matrix.pl; a successful build IS the verdict (the
# recipe script is self-verifying and fails the build on any miss).
#
#   docker build -f tools/install-matrix/Dockerfile \
#       --build-arg BASE_IMAGE=debian:12 .

ARG BASE_IMAGE=ubuntu:24.04
FROM ${BASE_IMAGE}

# The 2.6.0 sbcl.org binary needs glibc >= 2.38; older bases pin 2.5.2 (the
# validated floor).  tools/install-matrix.pl passes the right one per image.
ARG SBCL_VERSION=2.6.0
ENV SBCL_VERSION=${SBCL_VERSION}

COPY . /pcl
WORKDIR /pcl

RUN tools/install-matrix/install-and-verify.sh
