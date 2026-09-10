# Percolisp (PCL) in a container — `docker run --rm ghcr.io/percolisp/pcl -E 'say 6*7'`
#
# The point of this file is that trying PCL costs one command.  PCL needs a
# perl newer than a distro's PPI, and an SBCL newer than any current distro
# packages; that is two downloads and a version check before a person can see
# whether the thing is interesting at all.  An image removes all of it.
#
# THE RECIPE IS NOT DUPLICATED HERE.  The dependency half is
# tools/install-matrix/deps.sh — the same script .github/workflows/
# install-matrix.yml and tools/t/install-container.t run, so the image cannot
# drift from what CI proves installable (rule 11).  The install half is
# tools/install-pcl, which self-verifies: it refuses to finish unless the
# INSTALLED tools transpile and run a program, so a successful build is
# already a working PCL.
#
#   podman build -t percolisp/pcl .          # docker build is identical
#   podman run --rm percolisp/pcl -E 'say 6*7'
#   podman run --rm -v "$PWD":/work percolisp/pcl demo.pl
#
# tools/install-matrix/Dockerfile stays where it is and answers a different
# question ("does the recipe work on THIS distro", parametrized over four
# bases).  This one builds the artifact people run.

ARG BASE_IMAGE=ubuntu:24.04
FROM ${BASE_IMAGE}

# sbcl.org's 2.6.0 x86-64 binary needs glibc >= 2.38, which ubuntu:24.04 has.
# A build over an older base must pass --build-arg SBCL_VERSION=2.5.2, the
# validated floor (README "Requirements").
ARG SBCL_VERSION=2.6.0
ENV SBCL_VERSION=${SBCL_VERSION} \
    DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# The dependency layer, copied on its own so that editing PCL's source does
# not re-download SBCL.  $HOME points at a world-readable directory instead of
# root's home because the image runs as a non-root user: deps.sh puts SBCL
# under $HOME, and that user has to be able to read it.
COPY tools/install-matrix/deps.sh /tmp/deps.sh
RUN mkdir -p /opt/pcldeps \
 && HOME=/opt/pcldeps /tmp/deps.sh \
 && chmod -R a+rX /opt/pcldeps \
 && ln -sf /opt/pcldeps/sbcl/bin/sbcl /usr/local/bin/sbcl \
 && rm -rf /tmp/deps.sh /tmp/sbcl.tar.bz2 /tmp/sbcl-*-x86-64-linux /var/lib/apt/lists/*
ENV SBCL_HOME=/opt/pcldeps/sbcl/lib/sbcl

# The install.  /opt/pcl is the shared-install shape tools/t/install-container.t
# legs (c)/(d) already cover: read-only for everyone, each user's compiled-module
# cache under their own home.  The source tree is not kept — an installation is
# self-contained (`pcl --version` reads the VERSION file written here).
#
# PCL_VERSION: the build context has no .git (.dockerignore — it is large and
# changes every commit), so tools/install-pcl falls back to CHANGELOG.md's top
# heading and an image would call itself "Unreleased".  The release workflow
# passes the tag it is building, which is the only thing that actually knows.
# Left empty, the CHANGELOG answer stands, which is honest for a local build.
ARG PCL_VERSION=
COPY . /src
RUN cd /src \
 && tools/install-pcl --prefix /opt/pcl --quiet \
 && if [ -n "$PCL_VERSION" ]; then echo "$PCL_VERSION" > /opt/pcl/lib/pcl/VERSION; fi \
 && chmod -R a+rX /opt/pcl \
 && rm -rf /src
ENV PATH=/opt/pcl/bin:$PATH

# A non-root user, and /work as the place a bind-mounted script lands.
RUN useradd -m -u 1001 -s /bin/bash pcl && mkdir -p /work && chown pcl:pcl /work
USER pcl
WORKDIR /work

# `pcl` is the everyday command, so it is the entry point: arguments after the
# image name are pcl's own.  `--entrypoint bash` (or `--entrypoint pl2cl`) is
# there for anyone who wants the compiler or a shell instead.
ENTRYPOINT ["pcl"]
