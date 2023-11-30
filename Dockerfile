# syntax = docker/dockerfile:experimental

ARG PYTHON_IMAGE=python
ARG PYTHON_VERSION=3.10

FROM $PYTHON_IMAGE:$PYTHON_VERSION-slim as base

ENV DEBIAN_FRONTEND=noninteractive \
    LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONFAULTHANDLER=1 \
    PYTHONHASHSEED=random \
    PYTHONUNBUFFERED=1 \
    POETRY_VIRTUALENVS_CREATE=false \
    VENV_PATH="/opt/venv" \
    PATH="/opt/venv/bin:$PATH"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git make \
        curl ca-certificates gnupg apt-transport-https

ARG INCLUDE_DEV_DEPS=false

RUN if [ $INCLUDE_DEV_DEPS = "true" ]; then \
      # julia is no longer packaged in Debian 12
      # this curl install should be enough for a container:
      curl -s https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.4-linux-x86_64.tar.gz \
        | tar --strip-components=1 -xz -C /usr/local && \
      apt-get install -y --no-install-recommends \
        default-jdk-headless \
        fp-compiler \
        g++ \
        gambc \
        gcc \
        gdc \
        ghc \
        golang-go \
        lua5.3 \
        mono-mcs \
        nodejs \
        ocaml-nox \
        perl \
        php-cli \
        ruby \
        rustc \
        swi-prolog-nox && \
      apt-get install -y --no-install-recommends locales && \
      sed -i -e 's/# fr_FR.UTF-8 UTF-8/fr_FR.UTF-8 UTF-8/' /etc/locale.gen && \
      locale-gen; \
    fi

FROM base as builder

ARG INCLUDE_DEV_DEPS=false

RUN apt-get update && \
    apt-get install -y --no-install-recommends gcc git

RUN --mount=type=bind,target=./pyproject.toml,src=./pyproject.toml \
    --mount=type=bind,target=./poetry.lock,src=./poetry.lock \
    --mount=type=cache,target=/root/.cache/pypoetry \
    python -m venv /opt/venv && \
    pip3 install --upgrade pip && \
    pip3 install poetry && \
    poetry install $(if [ $INCLUDE_DEV_DEPS = "false" ]; then echo "--no-dev"; fi)

COPY ./ /iorgen
WORKDIR /iorgen
RUN poetry install

FROM base

COPY --from=builder /opt/venv/ /opt/venv/
COPY --from=builder /iorgen/ /iorgen/
WORKDIR /
