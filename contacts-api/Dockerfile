FROM futurice/base-images:haskell-lts-2.22
MAINTAINER Oleg Grenrus <oleg.grenrus@iki.fi>

# TEMPORARY
RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends --force-yes install \
    ca-certificates \
    curl \
    git \
    libfftw3-dev \
    libpq-dev \
    pkg-config \
  && rm -rf /var/lib/apt/lists/*

# Create user
RUN useradd -m -s /bin/bash -d /app app

# Install dependencies
WORKDIR /app/src
ADD stack*.yaml /app/src/

# Copy cabal files
ADD contacts.cabal /app/src/contacts.cabal

# Build dependencies
RUN stack build --only-dependencies

# Add rest and build the app
ADD . /app/src
RUN stack build
RUN cp $(stack path --local-install-root)/bin/contacts-server /app/contacts-server

# Finalise
RUN chown -R app:app /app

EXPOSE 8000

# Default startup command
USER app
WORKDIR /app
CMD ["/app/contacts-server", "+RTS", "-N", "-T"]
