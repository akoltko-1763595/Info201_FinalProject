@echo off

set /p msg="Enter Commit Message: "
git add .
git commit -m "%msg%"
git pull
echo "Up to date, close this window if merge issues else press space"
pause

git push
pause