#!/bin/bash


run_erl_command() {
  erl -sname server@10.31.2.112 -setcookie akash -eval "$1"
}


while true; do
  echo "Choose an operation:"
  echo "1. Create file"
  echo "2. Check server ping"
  echo "3. Retry file creation"
  echo "4. Write to a file"
  echo "5. Append to a file"
  echo "6. Check if a file exists"
  echo "7. Read a file"
  echo "8. List files"
  echo "9. Rename a file"
  echo "0. Exit"
  read -p "Enter your choice (0-9): " choice

  case $choice in
    1)

      read -p "Enter filename to create: " filename
      run_erl_command "fileClient:create('$filename')."
      echo "Created file '$filename'"
      ;;
    2)

      run_erl_command "net_adm:ping('server@10.31.2.112')."
      echo "Checked server ping"
      ;;
    3)

      read -p "Enter filename to retry creation: " filename
      run_erl_command "fileClient:create('$filename')."
      echo "Created '$filename' (retrying)"
      ;;
    4)

      read -p "Enter filename to write to: " filename
      read -p "Enter content to write: " content
      run_erl_command "fileClient:write('$filename', '$content')."
      echo "Written '$content' to '$filename'"
      ;;
    5)

      read -p "Enter filename to append to: " filename
      read -p "Enter content to append: " content
      run_erl_command "fileClient:append('$filename', '$content')."
      echo "Appended '$content' to '$filename'"
      ;;
    6)
      # Check if a file exists
      read -p "Enter filename to check if it exists: " filename
      run_erl_command "fileClient:exists('$filename')."
      echo "Checked if file '$filename' exists"
      ;;
    7)
      # Read a file
      read -p "Enter filename to read: " filename
      run_erl_command "fileClient:read('$filename')."
      echo "Read contents of '$filename'"
      ;;
    8)

      run_erl_command "fileClient:list()."
      echo "Listed files"
      ;;
    9)

      read -p "Enter current filename to rename: " old_filename
      read -p "Enter new filename: " new_filename
      run_erl_command "fileClient:rename('$old_filename', '$new_filename')."
      echo "Renamed '$old_filename' to '$new_filename'"
      ;;
    0)

      echo "Exiting script."
      exit 0
      ;;
    *)

      echo "Invalid choice. Please select a valid option."
      ;;
  esac
done
