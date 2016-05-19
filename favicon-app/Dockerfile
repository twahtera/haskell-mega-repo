FROM futurice/base-images:haskell-lts-2.22
MAINTAINER Oleg Grenrus <oleg.grenrus@iki.fi>

# Create user
RUN useradd -m -s /bin/bash -d /app app

# Install dependencies
WORKDIR /app/src
ADD stack.yaml /app/src/

# Copy cabal files
ADD *.cabal /app/src/

# Build dependencies
RUN stack build --only-dependencies

# Add rest and build the app
ADD . /app/src
RUN stack build
RUN cp $(stack path --local-install-root)/bin/favicon /app/favicon

# Finalise
RUN chown -R app:app /app

EXPOSE 8000

# Default startup command
USER app
WORKDIR /app
CMD ["/app/favicon", "+RTS", "-T", "-N"]
