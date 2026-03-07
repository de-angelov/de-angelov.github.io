---
title: "SSH Utilities Cheat Sheet"
tags: ["2026", "Linux"]
date: Mar 2, 2026
---

Secure Shell (SSH) is most commonly implemented via OpenSSH, the standard open-source SSH suite on Linux and macOS systems.

This post is a practical cheat sheet of core SSH commands **and related utilities like `ssh-copy-id`**, focused on daily sysadmin and developer workflows.

---

## 🔐 Core SSH Commands

### `ssh` — Remote Login

Connect to a remote machine:

<pre>
<code class="language-bash">
ssh user@host
ssh -p 2222 user@host
ssh -i ~/.ssh/id_ed25519 user@host
</code>
</pre>

Port forwarding examples:

<pre>
<code class="language-bash">
# Local forwarding
ssh -L 8080:localhost:80 user@host

# Remote forwarding
ssh -R 9090:localhost:3000 user@host

# Dynamic SOCKS proxy
ssh -D 1080 user@host
</code>
</pre>

### `ssh-keygen` — Key Management

Generate modern key:

<pre>
<code class="language-bash">
ssh-keygen -t ed25519 -C "you@example.com"
</code>
</pre>

Other useful operations:

<pre>
<code class="language-bash">
ssh-keygen -l -f id_ed25519.pub   # fingerprint
ssh-keygen -R hostname            # remove host from known_hosts
</code>
</pre>

### `ssh-copy-id` — Install Public Key

Copies your public key to a remote host’s `authorized_keys`.

<pre>
<code class="language-bash">
ssh-copy-id user@host
ssh-copy-id -i ~/.ssh/work.pub user@host
ssh-copy-id -p 2222 user@host
</code>
</pre>

👉 Only copies **public keys**, never private keys.


## 📂 File Transfer Utilities

### `scp` — Secure Copy

<pre>
<code class="language-bash">
scp file.txt user@host:/remote/path
scp -r folder user@host:/remote/path
scp user@host:/remote/file.txt .
</code>
</pre>


### `sftp` — Interactive File Transfer

<pre>
<code class="language-bash">
sftp user@host
</code>
</pre>

Inside session:

<pre>
<code class="language-bash">
put file.txt
get file.txt
ls
cd
</code>
</pre>


### `rsync` over SSH — Efficient Sync

<pre>
<code class="language-bash">
rsync -avz -e ssh folder/ user@host:/remote/folder
</code>
</pre>

Efficient for backups and deployments.


## 🔑 SSH Agent Utilities

### `ssh-agent` — Background Key Manager

<pre>
<code class="language-bash">
eval "$(ssh-agent -s)"
</code>
</pre>

### `ssh-add` — Load Keys Into Agent

<pre>
<code class="language-bash">
ssh-add ~/.ssh/id_ed25519
ssh-add -l
ssh-add -D
</code>
</pre>

Prevents repeated passphrase prompts.

## ⚙️ Server-Side Utilities

### `sshd` — SSH Daemon

Check status:

<pre>
<code class="language-bash">
systemctl status sshd
</code>
</pre>

Restart:

<pre>
<code class="language-bash">
sudo systemctl restart sshd
</code>
</pre>

Main config file:

<pre>
<code class="language-bash">
/etc/ssh/sshd_config
</code>
</pre>


## 🗂 Important SSH Files

Local (`~/.ssh/`):

- `id_ed25519` → private key
- `id_ed25519.pub` → public key
- `known_hosts` → trusted hosts
- `config` → client shortcuts

Remote:

- `~/.ssh/authorized_keys` → allowed public keys

Correct permissions matter:

<pre>
<code class="language-bash">
chmod 700 ~/.ssh
chmod 600 ~/.ssh/authorized_keys
</code>
</pre>


## 🧠 Useful Power Tricks

### Jump Host (Bastion)

<pre>
<code class="language-bash">
ssh -J user@jump user@internal-host
</code>
</pre>


### SSH Config Aliases

`~/.ssh/config`:

<pre>
<code class="language-bash">
Host prod
    HostName 10.0.0.10
    User deploy
    Port 2222
    IdentityFile ~/.ssh/prod_key
</code>
</pre>

Then simply:

<pre>
<code class="language-bash">
ssh prod
</code>
</pre>



## 🔐 Minimal Secure Setup Checklist

- Use `ed25519` keys
- Disable password authentication
- Disable root login
- Use non-standard port (optional)
- Use fail2ban (optional)
- Restrict users via `AllowUsers`


---

## 🧩 Mental Model

<p class="mermaid">
graph TD;
    A[Local Machine] -->|Private Key| B(SSH Client);
    B -->|Encrypted Channel| C[Remote SSH Server];
    C -->|authorized_keys| D(User Account);
</p>
