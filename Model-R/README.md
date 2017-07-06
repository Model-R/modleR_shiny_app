# Installation instructions

## The first time:
1. Download Docker https://www.docker.com/community-edition and install
2. Create your account and verify it
3. In a mac:
    4.  open the Docker app
    5. Open the terminal 
    6. Run `docker run modelr/shinyapp`
    7. The first time it will prompt a download of all the data in the container
    8. When it's over, open http://localhost:3838/Model-R/ in a web browser
    9. **If the app crashes or to close the session**: close the browser window, go to Terminal, type `docker ps`. It will give the list of current containers with a name for the current session such as `modest_darwin`
    10. Run `docker stop modest_darwin` and then `docker rm modest_darwin`. It will close the connection and remove it. 
    11. Repeat from step 6. 

The next times just start at step 5. 

**We'll provide instructions for Windows systems later but it should be similar**

# To run from Shiny and RStudio
1. Clone the Model-R repository https://github.com/Model-R/Model-R or download it and decompress the zip file
2. Open the server.R or the ui.R file in RStudio
3. Click on Run App inside the RStudio interface

