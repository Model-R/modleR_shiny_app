# Model-R shiny app

## Docker container installation

### The first time:
1. Download Docker https://www.docker.com/community-edition and install
2. Open the Docker app
3. Open a terminal window 
4. Run `docker run -p 3838:3838 -d modelr/shinyapp` - The first time it will prompt a download of all the data in the container
5. When it's over, open http://localhost:3838/Model-R/ in a web browser
6. **If the app crashes or to close the session**: close the browser window, go to Terminal, type `docker ps -a`. It will give the list of current containers with a binary name for them, such as `modest_darwin`
7. Run `docker stop modest_darwin` and then `docker rm modest_darwin` (replace with your own session names). It will close the connection and remove it. 
8. Repeat from step 4. 

### The next times 
1. Run `docker pull modelr/shinyapp` to make sure you have the latest version
2. Repeat from step 4 above.


## To run from Shiny and RStudio
1. Clone the Model-R repository https://github.com/Model-R/Model-R or download it and decompress the zip file
2. Open the server.R or the ui.R file in RStudio
3. Click on Run App inside the RStudio interface

