# website

fix for pushing 
# 1. Force-add the build folder (in case it's ignored)
git add . 

# 2. Commit it temporarily
git commit -m "Site update $(date)"

# 3. Push the subtree to gh-pages
git subtree push --prefix build/docs origin gh-pages

# 4. Remove the temporary commit from your 'main' branch
git reset HEAD~1