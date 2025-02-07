# Haskell Concurrent Web Server Simulation

## Overview
This project is a **Haskell Stack application** that simulates concurrent communication between a **web server** and **multiple web clients** using **threads** and **MVars**. The system models **HTTP-like requests and responses**, where clients send requests to a server, which processes them in FIFO order and responds accordingly.

## Features
 **Multi-threaded application**: One server and ten client threads.  
 **Random request generation**: Clients send requests at unpredictable intervals.  
 **FIFO request processing**: The server handles requests in the order they arrive.  
 **Timestamped logging**: All requests and responses include timestamps.  
 **Automatic termination**: The system stops after processing **100 request-response pairs**.  
 **Log file generation**: Outputs results to `requests.log`.  

## **Project Structure**

| **File**      | **Description**                                  |
|--------------|--------------------------------------------------|
| `Main.hs`    | Entry point, starts server and clients.         |
| `Server.hs`  | Implements web server logic.                    |
| `Client.hs`  | Defines client behavior.                        |
| `Types.hs`   | Defines `Request`, `Response`, and `RequestQueue`. |
| `Logger.hs`  | Handles request/response logging.               |

## How It Works
1. The **main program** spawns:
   - **1 Web Server thread**
   - **10 Client threads**
2. **Clients**:
   - Generate random requests (timestamped).
   - Send them to the server at **random intervals**.
3. **Server**:
   - Receives and **queues requests**.
   - Processes requests **FIFO (First-In-First-Out)**.
   - Sends responses back to clients.
4. **Logging**:
   - After **100 request-response cycles**, the system terminates.
   - A log file `requests.log` is generated.

## Installation & Setup
### **Prerequisites**
- **Haskell Stack** must be installed.

### **Steps to Run**
```sh
# Clone the repository
git clone <repository-url>
cd <project-directory>

# Build the project
stack build

# Run the application
stack run
```
## Libraries Used

- Control.Concurrent (forkIO, MVar) - Thread management.
- Data.Time (UTCTime, getCurrentTime) - Timestamps.
- System.Random (randomIO) - Random request generation.

## **License & Contact**

📜 **License:**  
This project is released under the **MIT License**.  
All rights reserved. You are free to use, modify, and distribute this project under the terms of the license.

📧 **Contact the Author:**  
For questions, suggestions, or contributions, feel free to **open an issue** on GitHub or reach out via email.

🔗 **Contributions & Issues:**  
- Pull requests are welcome! Please ensure your changes align with the project's objectives.  
- If you encounter any issues, **report them** on the GitHub Issues page.

🔒 **Disclaimer:**  
This software is provided **as is**, without warranty of any kind. The author is not responsible for any issues arising from its use.

© **zeynepkurtulus** – All Rights Reserved.

