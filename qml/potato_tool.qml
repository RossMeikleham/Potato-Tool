import QtQuick 2.2
import QtQuick.Dialogs 1.2
import QtQuick.Controls 1.4


ApplicationWindow {
    visible: true;
    width: 640;
    height: 480;
    title: "Potato Tool";


// Dialog for loading entire VMUs
FileDialog { 
    id: vmuLoadDialog;
    title: "Select VMU file";
    selectExisting: true;
    selectFolder: false;
    selectMultiple: false;
    folder: shortcuts.home;
    nameFilters: ["VMU files (*.vmu)", "All files (*)"];

    // Load the file
    onAccepted: {
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
    nameFilters: ["VMU files (*.vmu)", "All files (*)"];
    
    onAccepted: {
        close();
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
            }

            MenuItem {
                text: "Unlock Extra Blocks";
            }

            MenuItem {
                text: "Lock Extra Blocks";
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
    id: dummyModel;
    
    ListElement {
        image: "TODO";
        name: "Test Game";
        type: "GAME";
        blockCount: 32;
        startBlock: 0;
        created: "11/09/2015 00:02";
    } 
    
       
    ListElement {
        image: "TODO";
        name: "Test Data";
        type: "DATA";
        blockCount: 43;
        startBlock: 50;
        created: "03/04/2015 01:03";
    }    
}


TableView {
    model: dummyModel;
    selectionMode: SelectionMode.SingleSelection;
    
    anchors {
        left: parent.left;
        right: parent.right;
        bottom: parent.bottom;
        top: miscDetails.bottom;
    }
        
    
    // Icon image for the file
    TableViewColumn {
        role: "image";
        title: "Image";
        width: 200;
    }

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


    
    //Dialog for extracting individual files out of the VMU filesystem
    FileDialog { 
        id: fileExtractDialog;
        title: "Select location to extract file to";
        selectExisting: false;
        selectFolder: false;
        selectMultiple: false;
        folder: shortcuts.home;
        nameFilters: ["Nexus DCI format (*.dci)", "test format (*.test)"];

        // Save the file
        onAccepted: {
            close();
        }

        onRejected: {
            close();
        }
    }


    // Menu for performing actions on individual files within the filesystem
    Menu {
        id: contextMenu;
        
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
                    contextMenu.popup();
                }
            }       
        }
    }

}
}


/*
Rectangle {
    id: page
    width: 320; height: 480
    color: "lightgray"

    Text {
        id: helloText
        text: "Hello world!"
        y: 30
        anchors.horizontalCenter: page.horizontalCenter
        font.pointSize: 24; font.bold: true
    }
}
*/
