ARG VARIANT="jammy"
FROM mcr.microsoft.com/devcontainers/base:${VARIANT}

# Install system dependencies
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        libpq-dev \
        entr \
        libgmp-dev \
        esbuild \
        wget \
        curl \
        gnupg \
        build-essential \
        zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

    
    # Install Task runner
    ARG TASK_VERSION="3.35.1"
    RUN wget -qO /tmp/task.deb "https://github.com/go-task/task/releases/download/v${TASK_VERSION}/task_linux_amd64.deb" \
    && dpkg -i /tmp/task.deb \
    && rm /tmp/task.deb
    
    # Install GHCup (Haskell toolchain installer) non-interactively
    # Set environment variables for non-interactive installation and specific choices
    ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
    ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
    ENV BOOTSTRAP_HASKELL_ENABLE_STACK_INTEGRATION=1
    ENV BOOTSTRAP_HASKELL_SELECT_GHCUP_CHANNEL=G
    ENV BOOTSTRAP_HASKELL_ENABLE_PRERELEASES=0
    ENV BOOTSTRAP_HASKELL_ENABLE_CROSS=0
    
    
USER vscode

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/home/vscode/.ghcup/bin:/home/vscode/.cabal/bin:${PATH}"

RUN ghcup install ghc 9.6.7 \
    && ghcup install cabal 3.14.2.0 \
    && ghcup install hls 2.13.0.0 \
    && ghcup set ghc 9.6.7 

RUN cabal install hlint-3.8 \
    && cabal install ormolu-0.7.8.0
