@echo off
git pull
echo "Up to date, starting push"
pause

set /p msg="Enter Commit Message: "
git add .
git commit -m "%msg%"
git push
pause