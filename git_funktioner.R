# -------------------------------
# Git helper functions in R
# -------------------------------

# Commit & push changes
git_push <- function(message = "update") {
  system("git add .")
  system(sprintf('git commit -m "%s"', message))
  system("git push")
}

# Pull latest changes from remote
git_pull <- function() {
  system("git pull")
}

# Check repo status
git_status <- function() {
  system("git status")
}

# Show remotes
git_remotes <- function() {
  system("git remote -v")
}

# Configure username/email (once globally)
git_config <- function(name, email) {
  system(sprintf('git config --global user.name "%s"', name))
  system(sprintf('git config --global user.email "%s"', email))
}

# Initialize a new repo (only once in a new folder)
git_init <- function(remote_url) {
  system("git init")
  system("git branch -M main")
  system(sprintf("git remote add origin %s", remote_url))
}


# Commit and push with a message
git_push("added boverket map script")

# Pull updates
git_pull()

# Check repo status
git_status()

# See remotes
git_remotes()
