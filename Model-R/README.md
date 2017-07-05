# Installation instructions

## The first time:
1. Download Docker https://www.docker.com/community-edition and install
2. Create your account and verify it
3. Run Docker locally and login
4. Clone the Model-R repository https://github.com/Model-R/Model-R or download it and decompress the zip file
5. Open the terminal in the folder of the repository
6. Run `docker run modelr/shinyapp`
7. The first time it will prompt a donwload of all the data in the container
8. When it's over, open http://localhost:3838/Model-R/ in a web browser
9. **If the app crashes**, close the browser window, go to Terminal, type `docker ps`. It will give the list of current containers with a name for the current session such as `modest_darwin`
10. Run `docker stop modest_darwin` and then `docker rm modest_darwin`. It will close the connection and remove it. 
11. Repeat from step 6. 

The next times just start at step 5. 

