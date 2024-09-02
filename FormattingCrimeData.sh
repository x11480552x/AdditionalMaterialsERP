#!/bin/bash

# Define the start and end directories
start_dir="2023-05"
end_dir="2024-04"
destination_dir="filtered_files"

# Create destination directory if it doesn't exist
mkdir -p "$destination_dir"

# Define the list of police provinces
provinces=("avon-and-somerset" "bedfordshire" "btp" "cambridgeshire" "cheshire"
           "city-of-london" "cleveland" "cumbria" "derbyshire" "devon-and-cornwall"
           "dorset" "durham" "dyfed-powys" "essex" "gloucestershire"
           "gwent" "hampshire" "hertfordshire" "humberside" "kent"
           "lancashire" "leicestershire" "lincolnshire" "merseyside" "metropolitan"
           "norfolk" "north-wales" "north-yorkshire" "northamptonshire" "northern-ireland"
           "northumbria" "nottinghamshire" "south-wales" "south-yorkshire"
           "staffordshire" "suffolk" "surrey" "sussex" "thames-valley"
           "warwickshire" "west-mercia" "west-midlands" "west-yorkshire" "wiltshire")

# Define the output file path for combined CSV data
output_file="$destination_dir/combined_filtered_crime_data.csv"

# Write header to output file
echo "Longitude,Latitude,LSOA.code,Crime.ID,Crime.Month,Crime.Type" > "$output_file"

# Function to check if a directory is within the specified range
within_range() {
    current_dir=$1
    if [[ "$current_dir" > "$end_dir" ]]; then
        return 1
    fi
    if [[ "$current_dir" < "$start_dir" ]]; then
        return 1
    fi
    return 0
}

# Iterate over directories within the range
for dir in */; do
    # Remove trailing slash
    dir=${dir%/}

    # Check if the directory is within the range
    if within_range "$dir"; then
        echo "Processing directory: $dir"
       
        # Iterate over each police province
        for province in "${provinces[@]}"; do
            # Find the CSV file for the current province
            csv_file="$dir/$dir-$province-street.csv"
           
            # Check if the CSV file exists
            if [[ -f "$csv_file" ]]; then
                echo "Processing file: $csv_file"
               
                # Extract necessary columns (Longitude, Latitude, LSOA code, Crime ID, Crime Month, Crime Type)
                # for specified crime types and append to the combined output file
                awk -F, '
                    $10 == "Violence and sexual offences" ||
                    $10 == "Criminal damage and arson" ||
                    $10 == "Anti-social behaviour" ||
                    $10 == "Public order" {
                        print $5","$6","$8","$1","$2","$10
                    }
                ' "$csv_file" >> "$output_file"
            else
                echo "File not found: $csv_file"
            fi
        done
    fi
done

echo "All files have been processed and combined into $output_file."