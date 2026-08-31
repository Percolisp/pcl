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

COPY . /pcl
WORKDIR /pcl

RUN tools/install-matrix/install-and-verify.sh
