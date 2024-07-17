#!/bin/bash

PATH="/opt/local/bin:$PATH"
FILES=("Colloquium" "Experimental" "Soutenances")

for REF in "${FILES[@]}"; do
    export TITLE="Lab Calendar: ${REF}"
    export FILETAGS="$(echo $REF | tr '[:upper:]' '[:lower:]')"

    curl "https://www.salort.eu/seminars/$REF.ics" --output "$HOME/nextcloud/org/external/$REF.ics"
    $HOME/bin/ical2org.awk < "$HOME/nextcloud/org/external/$REF.ics" > "$HOME/nextcloud/org/external/$FILETAGS.org"
done
