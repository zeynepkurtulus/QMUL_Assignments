# Haskell Web Data Harvester

## Overview
This project is a **Haskell Stack-based application** that retrieves data from a **web API**, processes it, and stores it in a **SQLite database**. Users can also query and extract data from the database. The project is designed for **team collaboration** and should be managed using Git for version control.


## Features
 **Data Harvesting**: Fetch data from an assigned JSON-based web API.  
 **Data Parsing**: Convert raw JSON data into structured Haskell data types.  
 **Database Integration**: Store retrieved data in an SQLite database.  
 **Query System**: Users can execute predefined queries on the database.  
 **JSON Export**: Convert database records back to a JSON format.  
 **Command-line Interface**: Various commands for interacting with the app.  
 **Extra Feature**: Implement an additional complex feature for full marks.

## Project Structure

| **File**       | **Description** |
|---------------|---------------|
| `Main.hs`     | Entry point, processes command-line arguments. |
| `Types.hs`    | Defines Haskell data types for the API data. |
| `Fetch.hs`    | Retrieves JSON data from the web API. |
| `Parse.hs`    | Parses JSON into Haskell data structures and exports JSON. |
| `Database.hs` | Manages database interactions (create, insert, query). |

## **Commands**

| Command | Action |
|---------|--------|
| `stack run -- create` | Create SQLite database and tables. |
| `stack run -- loaddata` | Download data from API and save to the database. |
| `stack run -- dumpdata` | Export database contents to `data.json`. |
| `stack run -- <query> <query_arguments>` | Execute a custom query on the database. |

## **Libraries Used**
- **Control.Concurrent** (for potential concurrency improvements)
- **Database.SQLite.Simple** (for database interactions)
- **Data.Aeson** (for JSON parsing)
- **Network.HTTP.Simple** (for fetching web data)

## Usage

### **Prerequisites**
Ensure you have **Haskell Stack** installed.

### **Setup & Execution**

```sh
# Clone the repository
git clone <repository-url>
cd <project-directory>
```
## License & Contact
📜 **License:** This project is licensed under the **MIT License**.  
📧 **Contact:** For queries, open an issue or contact the authors.  
🔗 **Contributions:** Pull requests and issue reports are welcome.  
🚀 **All rights reserved**. This software is provided "as is" without any warranties.

