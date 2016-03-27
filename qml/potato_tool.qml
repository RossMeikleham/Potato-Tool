import QtQuick 2.2
import QtQuick.Dialogs 1.2
import QtQuick.Controls 1.4


ApplicationWindow {
    visible: true;
    width: 640;
    height: 480;
    title: "Potato Tool";


Connections {
    target: self
    onVmuError : {
        errorDialog.text = msg;
        errorDialog.open();
    }

    onVmuInfo : {
        infoDialog.text = msg;
        infoDialog.open();
    }

    // Re-display everything
    onVmuChanged : {
        saveDataModel.clear(); 
       
        var formatDate = function(tStamp) {
            var yearLeft = tStamp.century;
            var yearStr = ((yearLeft * 100) + tStamp.year).toString();
            
    
            var toDateTimeNum = function(n) {
                return n < 10 ? "0" + n.toString() : n.toString();
            };

            var monthStr = toDateTimeNum(tStamp.month); 
            var dayStr = toDateTimeNum(tStamp.day);
            var hourStr = toDateTimeNum(tStamp.hour);
            var minStr = toDateTimeNum(tStamp.minute);
            var secStr = toDateTimeNum(tStamp.second);

            return dayStr + "/" + monthStr + "/" + yearStr + "  " +
                   hourStr + ":" + minStr + ":" + secStr;
        }

        for (var i = 0; i < vmu.files.length; i++) {
            saveDataModel.append({
                "name": vmu.files[i].fileName,
                "type": vmu.files[i].fileType,
                "blockCount": vmu.files[i].blocks,
                "startBlock": vmu.files[i].startBlock,
                "created": formatDate(vmu.files[i].timestamp)
            });
        }
    }
}

// Error Dialog
MessageDialog {
    id: errorDialog;
    title: "Error";
    icon: StandardIcon.Critical;
    standardButtons: StandardButton.Ok;
    onAccepted: {
        errorDialog.close();
    }
    
}

// Info Dialog
MessageDialog {
    id: infoDialog;
    title: "Info";
    icon: StandardIcon.Information;
    standardButtons: StandardButton.Ok;
    onAccepted: {
        infoDialog.close();       
    }
}

// Dialog for loading entire VMUs
FileDialog { 
    id: vmuLoadDialog;
    title: "Select VMU file";
    selectExisting: true;
    selectFolder: false;
    selectMultiple: false;
    folder: shortcuts.home;
    nameFilters: ["VMU files (*.bin)", "All files (*)"];

    // Load the file
    onAccepted: {
        var path = fileUrl.toString().replace("file://","");
        openVMU(path);
        close();
    }

    onRejected: {
        close();
    }
}


// Dialog for saving entire VMUs
FileDialog {
    id: vmuSaveDialog;
    title: "Select path to save VMU file";
    selectExisting: false;
    selectFolder: false;
    selectMultiple: false;
    folder: shortcuts.home;
    nameFilters: ["VMU files (*.bin)", "All files (*)"];
    
    onAccepted: {
        close();
        var path = fileUrl.toString().replace("file://","");
        saveVMU(path);
    }

    onRejected: {
        close();
    }
}

// Dialog for loading individual VMU save files
FileDialog {
    id: vmuFileSaveDialog;
    title: "Select path to load VMU save file";
    selectExisting: true;
    selectFolder: false;
    selectMultiple: false;
    folder: shortcuts.home;
    nameFilters: ["Nexus DCI files (*.dci)", "All files (*)"];
    
    onAccepted: {
        close();
        var path = fileUrl.toString().replace("file://","");
        addVMUSaveFile(path);
    }

    onRejected: {
        close();
    }
}

    menuBar : MenuBar {
        Menu {
            title: "File"
            
            MenuItem {
                text: "New VMU";
                onTriggered : {
                    createNewVMU();
                }
            }

            MenuItem {
                text: "Save VMU";
                onTriggered : {
                    vmuSaveDialog.open();
                }
            }

            MenuItem {
                text: "Load VMU";
                onTriggered: {
                    vmuLoadDialog.open();
                }
            }

            MenuItem {
                text: "Quit";
                onTriggered: {
                    Qt.quit();
                }
            }
        }

        Menu {
            title: "Edit"

            MenuItem {
                text: "Import Save";
                onTriggered: {
                    vmuFileSaveDialog.open();
                }        
            }

            MenuItem {
                text: "Unlock Unused Blocks";
                onTriggered: {
                    unlockUnusedBlocks();
                }
            }

            MenuItem {
                text: "Lock Unused Blocks";
                onTriggered: {
                    lockUnusedBlocks();
                }
            }
        } 
    }



// Miscellaneous VMU details
Rectangle {
    id: miscDetails;
    height: 150;

    anchors {
        left: parent.left;
        right:parent.right;
        top: parent.top;
    }
}


// Dummy List Model to populate the UI for now
ListModel {
    id: saveDataModel;
}    

TableView {
    model: saveDataModel;
    selectionMode: SelectionMode.SingleSelection;
    
    anchors {
        left: parent.left;
        right: parent.right;
        bottom: parent.bottom;
        top: miscDetails.bottom;
    }
        
    
    // Icon image for the file
/*    TableViewColumn {
        role: "image";
        title: "Image";
        width: 200;
    }
*/
    // File Name
    TableViewColumn {
        role: "name";
        title: "Name";
        width: 100;
    }

    // "Type" of file either GAME or DATA
    TableViewColumn {
        role: "type";
        title: "Type";
        width: 100;
    }

    // Number of Blocks the file takes up
    TableViewColumn {
        role: "blockCount";
        title: "Blocks";
        width: 100;
    }

    // Block number the file starts At
    TableViewColumn {
        role: "startBlock";
        title: "Start Block";
        width: 100;
    }
   
    // Date the file was created 
    TableViewColumn {
        role: "created";
        title: "Created";
        width: 150;
    }


        // Dialog for extracting individual VMU files to disk
        FileDialog {
            id: fileExtractDialog;
            title: "Select path to save individual VMU file";
            selectExisting: false;
            selectFolder: false;
            selectMultiple: false;
            folder: shortcuts.home;
            nameFilters: ["Nexus DCI format (*.dci)"];
            
            onAccepted: {
                close();
                var path = fileUrl.toString().replace("file://","");
                saveVMUSaveFile(contextMenu.itemSelected, path);
            }

            onRejected: {
                close();
            }
        }


    // Menu for performing actions on individual files within the filesystem
    Menu {
        id: contextMenu;
        property int itemSelected;

        
        // Extract the selected file from the VMU filesystem
        MenuItem {
            text: "Extract";
            onTriggered : {
                fileExtractDialog.open();
            }
        }
        
        // Move the selected file to another block in the filesystem
        MenuItem {
            text: "Move To Block";
        }

        // Delete the selected file from the filesystem
        MenuItem {
            text: "Delete";
            onTriggered : {
               removeSaveFile(contextMenu.itemSelected); 
            }
        }
    }

    rowDelegate: Item {

        Rectangle {
       
            anchors {
                left: parent.left;
                right: parent.right;
                verticalCenter: parent.verticalCenter;
            }
       
            height: parent.height;
            color: styleData.selected ? 'lightblue' : (styleData.alternate ? 'lightgray' : 'white');
       
            // "Right" clicking on a row brings up the file menu
            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.RightButton
                onClicked: {
                    contextMenu.itemSelected = styleData.row + 1;
                    contextMenu.popup();
                }
            }       
        }
    }

}
}
