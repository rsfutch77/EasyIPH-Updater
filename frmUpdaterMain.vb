
Imports System.IO
Imports System.Xml
Imports System.Net
Imports System.ComponentModel
Imports System.Data.SQLite
Imports System.Globalization ' For culture info
Imports System.Threading
Imports Ionic.Zip

Delegate Sub UpdateStatusSafe(ByVal pgBarVisible As Boolean, ByVal lblText As String)
Delegate Sub UpdatePGBarSafe(ByVal pgBarValue As Integer)

Public Class frmUpdaterMain

    Public Structure FileEntry
        Dim Name As String
        Dim Version As String
        Dim URL As String
        Dim MD5 As String
    End Structure

    Private Enum SettingTypes
        TypeInteger = 1
        TypeDouble = 2
        TypeString = 3
        TypeBoolean = 4
        TypeLong = 5
    End Enum


    Private Const AppSettingsFileName As String = "ApplicationSettings"
    Private Const SettingsFolder As String = "Settings/"
    Private Const XMLfileType As String = ".xml"
    Private Const DefaultProxyAddress As String = ""
    Private Const DefaultProxyPort As Integer = 0

#Region "Delegate Functions"

    Private Property HaveReprocessingFields As Boolean

    Public Sub UpdateStatus(ByVal pgBarVisible As Boolean, ByVal lblText As String)
        pgUpdate.Visible = pgBarVisible
        If lblText <> "" Then
            lblUpdateMain.Text = lblText
        End If
    End Sub

    ' Updates the value in the progressbar for a smooth progress (slows procesing a little) - total hack from this: http://stackoverflow.com/questions/977278/how-can-i-make-the-progress-bar-update-fast-enough/1214147#1214147
    Public Sub UpdateProgressBar(inValue As Integer)
        If inValue <= pgUpdate.Maximum - 1 And inValue <> 0 Then
            pgUpdate.Value = inValue
            pgUpdate.Value = pgUpdate.Value - 1
            pgUpdate.Value = inValue
        Else
            pgUpdate.Value = inValue
        End If
    End Sub

#End Region

    ' Worker
    Public worker As BackgroundWorker
    Public TestingVersion As Boolean ' For testing downloads from the server for a new update
    Public LocalXMLFileName As String

    Public UpdateFileList As New List(Of FileEntry) ' List of files that need updating, will download and rename all at the same time
    Public EVEImagesLocalFolderName As String = "" ' This is the name of the folder we are going to replace. This is stored in the text file on local comp

    Public Const XMLLatestVersionFileName As String = "LatestVersionIPH.xml"
    Public Const XMLLatestVersionTest As String = "LatestVersionIPH Test.xml"
    Public Const UpdaterFileName As String = "EVEIPH Updater.exe"

    ' File Path
    Public Const XMLUpdateServerURL = "http://www.mediafire.com/download/zazw6acanj1m43x/LatestVersionIPH.xml"
    Public Const XMLUpdateTestServerURL = "http://www.mediafire.com/download/zlkpaw8qck4qryw/LatestVersionIPH_Test.xml"

    ' For tracking an error
    Public ProgramErrorLocation As String
    Public SQL As String ' Keep global so I can put in error log
    Public ThrownError As String

    Public UPDATES_FOLDER As String ' Where Updates will take place 
    Public ROOT_FOLDER As String ' Where the root folder is located
    Public EVEIPH_SHELL_PATH As String ' Where to shell back to

    Public Const EVE_DB As String = "EVEIPH DB.s3db"
    Public Const EVE_IMAGES_ZIP As String = "EVEIPH Images.zip"
    Public Const EVEIPH_EXE As String = "EVE Isk per Hour.exe" ' For Shelling

    Public Const DATASOURCESTRING As String = "Data source="

    Public Const NO_LOCAL_XML_FILE As String = "NO LOCAL XML FILE"

    Public Const OLD_PREFIX As String = "OLD_"

    Public LocalCulture As New CultureInfo("en-US")

    Public Sub New()
        Dim UserPath As String = ""

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        ' Update folder path
        UPDATES_FOLDER = "Updates\"
        ROOT_FOLDER = ""

        ' Set test platform
        If File.Exists("Test.txt") Then
            TestingVersion = True
        Else
            TestingVersion = False
        End If

        EVEIPH_SHELL_PATH = ROOT_FOLDER & EVEIPH_EXE

        ' Set the version of the XML file we will use
        If TestingVersion Then
            LocalXMLFileName = XMLLatestVersionTest
        Else
            LocalXMLFileName = XMLLatestVersionFileName
        End If

        ' Create the updates folder
        If Directory.Exists(UPDATES_FOLDER) Then
            ' Delete what is there and replace
            Dim ImageDir As New DirectoryInfo(UPDATES_FOLDER)
            ImageDir.Delete(True)
        End If

        ' Create the new folder
        Directory.CreateDirectory(UPDATES_FOLDER)

        BGWorker.WorkerReportsProgress = True
        BGWorker.WorkerSupportsCancellation = True

        pgUpdate.Value = 0
        pgUpdate.Visible = False
        pgUpdate.Maximum = 100

        ProgramErrorLocation = ""
        ThrownError = ""

        Me.Focus()

    End Sub

    ' This event handler is where the time-consuming work is done.
    Private Sub BGWorker_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BGWorker.DoWork
        worker = CType(sender, BackgroundWorker)
        Dim ProgressCounter As Integer

        Dim m_xmld As New XmlDocument
        Dim m_nodelist As XmlNodeList
        Dim m_node As XmlNode

        Dim LocalFileMD5 As String = ""
        Dim EVEImagesNewLocalFolderName As String = "" ' This is the name of the folder we are going to unzip to
        Dim EVEDBLocalFileVersion As String = "" ' Local DB version

        Dim TempFile As FileEntry
        Dim ServerFileList As New List(Of FileEntry)
        Dim LocalFileList As New List(Of FileEntry)

        Dim i, j As Integer
        Dim RecordCount As Integer
        Dim CheckFile As String ' For checking if the file downloads or not
        Dim UpdateComplete As Boolean = False

        ' XML Temp file path for server file
        Dim ServerXMLLastUpdatePath As String

        ' For DB updates
        Dim DBCommand As SQLiteCommand
        Dim readerUpdate As SQLiteDataReader
        Dim readerCheck As SQLiteDataReader
        Dim readerCheck2 As SQLiteDataReader
        Dim DBOLD As New SQLiteConnection
        Dim DBNEW As New SQLiteConnection

        ' Delegate for updating status
        Dim UpdateStatusDelegate As UpdateStatusSafe

        Dim VersionNumber As Double = 0

        Dim TempAccessMask As String
        Dim TempExpDate As Date
        Dim TempAccountType As String

        Dim HavePrecentiles As Boolean = False
        Dim HaveNewAPIFields As Boolean = False
        Dim HaveNewEVEIPHFields As Boolean = False
        Dim HasOldOutpostDataField As Boolean = False
        Dim HaveNewIndustryJobsTable As Boolean = False
        Dim HaveNewItemPricesFields As Boolean = False
        Dim HaveNewOwnedBPTable As Boolean = False

        'Create a variable tracking times
        Dim Count As Long = 0
        Dim Counter As Long = 0

        '================================================
        On Error GoTo 0

        Application.UseWaitCursor = True

        ' Sets the CurrentCulture 
        Thread.CurrentThread.CurrentCulture = LocalCulture

        UpdateStatusDelegate = New UpdateStatusSafe(AddressOf UpdateStatus)
        Me.Invoke(UpdateStatusDelegate, False, "Checking for Updates...")

        ' Get the newest update file from server
        If TestingVersion Then
            ServerXMLLastUpdatePath = DownloadFileFromServer(XMLUpdateTestServerURL, UPDATES_FOLDER & LocalXMLFileName)
        Else
            ServerXMLLastUpdatePath = DownloadFileFromServer(XMLUpdateServerURL, UPDATES_FOLDER & LocalXMLFileName)
        End If

        If ServerXMLLastUpdatePath <> "" Then
            ' Load the server xml file to check for updates 
            m_xmld.Load(ServerXMLLastUpdatePath)

            m_nodelist = m_xmld.SelectNodes("/EVEIPH/result/rowset/row")

            ' Loop through the nodes 
            For Each m_node In m_nodelist
                ' Load all except updater
                If m_node.Attributes.GetNamedItem("Name").Value <> UpdaterFileName Then
                    TempFile.Name = m_node.Attributes.GetNamedItem("Name").Value
                    TempFile.Version = m_node.Attributes.GetNamedItem("Version").Value
                    TempFile.MD5 = m_node.Attributes.GetNamedItem("MD5").Value
                    TempFile.URL = m_node.Attributes.GetNamedItem("URL").Value
                    ' Insert the file
                    ServerFileList.Add(TempFile)
                End If
            Next
        Else
            ' Didn't download properly
            GoTo RevertToOldFileVersions
        End If

        If File.Exists(ROOT_FOLDER & LocalXMLFileName) Then
            ' Load the local xml file to check for updates for the DB and images
            m_xmld.Load(ROOT_FOLDER & LocalXMLFileName)
            m_nodelist = m_xmld.SelectNodes("/EVEIPH/result/rowset/row")

            ' Loop through the nodes 
            For Each m_node In m_nodelist
                ' Load all except updater
                If m_node.Attributes.GetNamedItem("Name").Value <> UpdaterFileName Then
                    TempFile.Name = m_node.Attributes.GetNamedItem("Name").Value
                    TempFile.Version = m_node.Attributes.GetNamedItem("Version").Value
                    TempFile.MD5 = m_node.Attributes.GetNamedItem("MD5").Value
                    TempFile.URL = m_node.Attributes.GetNamedItem("URL").Value
                    ' Insert the file
                    LocalFileList.Add(TempFile)
                End If
            Next
        End If

        ' Done with these
        m_xmld = Nothing
        m_nodelist = Nothing
        m_node = Nothing

        Me.Invoke(UpdateStatusDelegate, False, "Downloading Updates...")
        Application.DoEvents()

        ' Now download all in the list if the server has newer versions
        RecordCount = ServerFileList.Count - 1
        For i = 0 To RecordCount

            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit Sub
            End If

            ' Get the MD5 from each filename in the list and compare to XML, if different, download the update
            If ServerFileList(i).Name = EVE_IMAGES_ZIP Or ServerFileList(i).Name = EVE_DB Then
                ' Zip file of images or the DB, so special processing
                ' Zip file is in a folder after update and DB will have a different MD5 after it is updated
                ' Need to load the local MD5 data from the Local XML since the folder doesn't have one MD5
                If Not IsNothing(LocalFileList) Then
                    For j = 0 To LocalFileList.Count - 1
                        ' Find the MD5 for the EVEDB or Image Zip file
                        If ServerFileList(i).Name = LocalFileList(j).Name Then
                            ' For the zip file, save the name of the current image folder (based on xml file)
                            If ServerFileList(i).Name = EVE_IMAGES_ZIP Then
                                EVEImagesLocalFolderName = LocalFileList(j).Name.Substring(0, Len(LocalFileList(j).Name) - 4)
                            ElseIf ServerFileList(i).Name = EVE_DB Then
                                EVEDBLocalFileVersion = LocalFileList(j).Version
                            End If

                            LocalFileMD5 = LocalFileList(j).MD5
                            Exit For
                        End If
                    Next
                Else
                    LocalFileMD5 = ""
                    EVEImagesLocalFolderName = EVE_IMAGES_ZIP.Substring(0, Len(EVE_IMAGES_ZIP) - 4)
                    EVEDBLocalFileVersion = EVE_DB
                End If
            Else
                ' All files other than the DB and Updater are run from the Root folder - Images and db a special exception above
                LocalFileMD5 = MD5CalcFile(ROOT_FOLDER & ServerFileList(i).Name)
            End If

            ' Compare the MD5's and see if we download the new file
            If LocalFileMD5 <> ServerFileList(i).MD5 Then

                ' Need to update, download to updates folder for later update
                Me.Invoke(UpdateStatusDelegate, True, "")
                CheckFile = DownloadFileFromServer(ServerFileList(i).URL, UPDATES_FOLDER & ServerFileList(i).Name)

                If (worker.CancellationPending = True) Then
                    e.Cancel = True
                    Exit Sub
                End If

                If CheckFile = "" Then
                    ' Some error in downloading
                    ProgramErrorLocation = "Download Failed."
                    Exit Sub
                Else
                    ' Check the file MD5 to make sure we got a good download. If not, try one more time
                    ' If they don't have a local file (which will have a blank MD5) then just go with what they got
                    If ServerFileList(i).MD5 <> NO_LOCAL_XML_FILE Then
                        ' Get the file size to check
                        Dim infoReader As System.IO.FileInfo
                        infoReader = My.Computer.FileSystem.GetFileInfo(CheckFile)
                        ' Still bad MD5 or the file is 0 bytes
                        If MD5CalcFile(CheckFile) <> ServerFileList(i).MD5 Or infoReader.Length = 0 Then
                            CheckFile = DownloadFileFromServer(ServerFileList(i).URL, UPDATES_FOLDER & ServerFileList(i).Name)

                            If (worker.CancellationPending = True) Then
                                e.Cancel = True
                                Exit Sub
                            End If

                            If MD5CalcFile(CheckFile) <> ServerFileList(i).MD5 Or CheckFile = "" Then
                                ProgramErrorLocation = "Download Corrupted."
                                Exit Sub
                            End If
                        End If
                    End If
                End If
                ' Record the file we are upating
                UpdateFileList.Add(ServerFileList(i))
            End If

            Me.Invoke(UpdateStatusDelegate, False, "")
        Next

        ' Leave if nothing to update
        If IsNothing(UpdateFileList) Then
            Exit Sub
        End If

        Me.Invoke(UpdateStatusDelegate, False, "Installing Updates...")
        Application.DoEvents()

        ' Try to update the old files, delete, and rename to new
        RecordCount = UpdateFileList.Count - 1
        For i = 0 To RecordCount

            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit Sub
            Else
                ' Report progress.
                If RecordCount > 0 Then
                    worker.ReportProgress((i / RecordCount) + 1 * 10)
                End If
            End If

            ' Now that we have the files downloaded, run special updates for DB and images (Zipped), the others are just saved already
            If UpdateFileList(i).Name.Substring(UpdateFileList(i).Name.Length - 5) = ".s3db" Then

                ' Copy the following tables before renaming
                ' API
                ' ASSETS
                ' ASSET_LOCATIONS
                ' CHARACTER_CORP_ROLES
                ' CHARACTER_CORP_TITLES
                ' CHARACTER_IMPLANTS
                ' CHARACTER_JUMP_CLONES
                ' CHARACTER_SHEET
                ' CHARACTER_SKILLS
                ' CHARACTER_STANDINGS
                ' CURRENT_RESEARCH_AGENTS

                ' EMD_ITEM_PRICE_HISTORY
                ' EMD_UPDATE_HISTORY
                ' MARKET_HISTORY
                ' MARKET_HISTORY_UPDATE_CACHE
                ' MARKET_ORDERS
                ' MARKET_ORDERS_UPDATE_CACHE
                ' PRICE_PROFILES

                ' CREST_CACHE_DATES
                ' INDUSTRY_JOBS
                ' ITEM_PRICES
                ' ITEM_PRICES_CACHE
                ' OWNED_BLUEPRINTS
                ' STATIONS
                ' FW_SYSTEM_UPGRADES

                ' NEW CREST TABLES
                ' INDUSTRY_CATEGORY_SPECIALTIES
                ' INDUSTRY_FACILITIES
                ' INDUSTRY_GROUP_SPECIALTIES
                ' INDUSTRY_SYSTEMS_COST_INDICIES
                ' INDUSTRY_TEAMS
                ' INDUSTRY_TEAMS_AUCTIONS
                ' INDUSTRY_TEAMS_BONUSES
                ' STATION_FACILITIES - Copy this data so we don't have to build it later

                ' Open databases, if no database file, so just exit and use downloaded one
                If File.Exists(ROOT_FOLDER & UpdateFileList(i).Name) Then
                    DBOLD.ConnectionString = DATASOURCESTRING & ROOT_FOLDER & UpdateFileList(i).Name
                    DBOLD.Open()

                    DBNEW.ConnectionString = DATASOURCESTRING & UPDATES_FOLDER & UpdateFileList(i).Name
                    DBNEW.Open()

                    Call ExecuteNonQuerySQL("PRAGMA synchronous = NORMAL", DBOLD)
                    Call ExecuteNonQuerySQL("PRAGMA synchronous = NORMAL", DBNEW)
                    'Call ExecuteNonQuerySQL("PRAGMA synchronous = OFF; PRAGMA locking_mode = EXCLUSIVE; PRAGMA cache_size = 10000; PRAGMA page_size = 4096; PRAGMA temp_store = MEMORY; PRAGMA journal_mode = OFF; PRAGMA count_changes = OFF", DBNEW)
                    Call ExecuteNonQuerySQL("PRAGMA auto_vacuum = FULL;", DBNEW) ' Keep the DB small

                    ' API
                    ProgramErrorLocation = "Cannot copy Character API"

                    ' See if they have the facility and blueprint cache field values first
                    On Error Resume Next
                    SQL = "SELECT FACILITIES_CACHED_UNTIL FROM API"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerCheck = DBCommand.ExecuteReader
                    ' If it didn't error, they have the fields
                    If Err.Number = 0 Then
                        HaveNewAPIFields = True
                        readerCheck.Close()
                    Else
                        HaveNewAPIFields = False
                    End If
                    On Error GoTo 0

                    readerCheck = Nothing

                    ' Now see if they have the new API table
                    On Error Resume Next
                    SQL = "SELECT 'X' FROM API"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    If Not IsNothing(readerUpdate) Then

                        ProgramErrorLocation = "Cannot copy API"
                        SQL = "SELECT * FROM API"

                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerUpdate = DBCommand.ExecuteReader

                        Call BeginSQLiteTransaction(DBNEW)
                        While readerUpdate.Read

                            SQL = "INSERT INTO API VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                            If HaveNewAPIFields Then
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16))
                            Else
                                SQL = SQL & "NULL,"
                                SQL = SQL & "NULL"
                            End If

                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()

                    Else
                        ProgramErrorLocation = "Cannot copy API"
                        SQL = "SELECT CACHED_UNTIL, USER_ID, API_KEY, API_TYPE, ACCESS_MASK, CHARACTER_ID, CHARACTER_NAME, "
                        SQL = SQL & "CORPORATION_ID, CORPORATION_NAME, OVERRIDE_SKILLS, DEFAULT_CHARACTER, "
                        SQL = SQL & "KEY_EXPIRATION_DATE, ASSETS_CACHED_UNTIL, INDUSTRY_JOBS_CACHED_UNTIL, "
                        SQL = SQL & "RESEARCH_AGENT_CACHED_UNTIL, FACILITIES_CACHED_UNTIL, BLUEPRINTS_CACHED_UNTIL FROM CHARACTER_API"

                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerUpdate = DBCommand.ExecuteReader

                        Call BeginSQLiteTransaction(DBNEW)
                        While readerUpdate.Read

                            SQL = "INSERT INTO API VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14))
                            SQL = SQL & ",NULL, NULL)" ' For new facilities and blueprints cache values

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()

                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' ASSETS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Assets"
                    SQL = "SELECT * FROM ASSETS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO ASSETS (ID, ItemID, LocationID, TypeID, Quantity, Flag, Singleton, RawQuantity) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' ASSET_LOCATIONS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Asset Locations"
                    SQL = "SELECT * FROM ASSET_LOCATIONS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO ASSET_LOCATIONS (EnumAssetType, ID, LocationID, FlagID) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' CHARACTER_CORP_ROLES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Character Corporation Roles"
                    SQL = "SELECT * FROM CHARACTER_CORP_ROLES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CHARACTER_CORP_ROLES (CHARACTER_ID, ROLE_TYPE, ROLE_ID, ROLE_NAME) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    End If

                    ' CHARACTER_CORP_TITLES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Character Corporation Titles"
                    SQL = "SELECT * FROM CHARACTER_CORP_TITLES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CHARACTER_CORP_TITLES (CHARACTER_ID, TITLE_ID, TITLE_NAME) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    End If

                    ' CHARACTER_IMPLANTS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Character Implants"
                    SQL = "SELECT * FROM CHARACTER_IMPLANTS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CHARACTER_IMPLANTS (CHARACTER_ID, JUMP_CLONE_ID, IMPLANT_ID, IMPLANT_NAME) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    End If

                    ' CHARACTER_JUMP_CLONES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Character Jump Clones"
                    SQL = "SELECT * FROM CHARACTER_JUMP_CLONES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CHARACTER_JUMP_CLONES (CHARACTER_ID, JUMP_CLONE_ID, LOCATION_ID, CLONE_NAME) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    End If

                    ' CHARACTER_SHEET
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Character Sheet"
                    SQL = "SELECT * FROM CHARACTER_SHEET"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CHARACTER_SHEET (CHARACTER_ID, CHARACTER_NAME, HOME_STATION_ID, DOB, "
                            SQL = SQL & "RACE, BLOOD_LINE_ID, BLOOD_LINE, ANCESTRY_LINE_ID, ANCESTRY_LINE, GENDER, CORPORATION_NAME, "
                            SQL = SQL & "CORPORATION_ID, ALLIANCE_NAME, ALLIANCE_ID, FACTION_NAME, FACTION_ID, FREE_SKILL_POINTS, "
                            SQL = SQL & "FREE_RESPECS, CLONE_JUMP_DATE, LAST_RESPEC_DATE, LAST_TIMED_RESPEC, REMOTE_STATION_DATE, JUMP_ACTIVATION, "
                            SQL = SQL & "JUMP_FATIGUE, JUMP_LAST_UPDATE, BALANCE, INTELLIGENCE, MEMORY, WILLPOWER, PERCEPTION, CHARISMA) VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(18)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(19)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(20)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(21)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(22)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(23)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(24)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(25)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(26)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(27)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(28)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(29)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(30))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    End If

                    ' CHARACTER_SKILLS
                    ProgramErrorLocation = "Cannot copy Character Skills"
                    SQL = "SELECT * FROM CHARACTER_SKILLS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    Call BeginSQLiteTransaction(DBNEW)

                    While readerUpdate.Read
                        SQL = "INSERT INTO CHARACTER_SKILLS VALUES ("
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6))
                        SQL = SQL & ")"

                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                    End While

                    Call CommitSQLiteTransaction(DBNEW)
                    readerUpdate.Close()
                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' CHARACTER_STANDINGS
                    ProgramErrorLocation = "Cannot copy Character Standings"
                    SQL = "SELECT * FROM CHARACTER_STANDINGS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    Call BeginSQLiteTransaction(DBNEW)

                    While readerUpdate.Read
                        SQL = "INSERT INTO CHARACTER_STANDINGS VALUES ("
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4))
                        SQL = SQL & ")"

                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                    End While

                    Call CommitSQLiteTransaction(DBNEW)
                    readerUpdate.Close()
                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' CURRENT_RESEARCH_AGENTS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy CURRENT_RESEARCH_AGENTS Data table"
                    SQL = "SELECT * FROM CURRENT_RESEARCH_AGENTS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO CURRENT_RESEARCH_AGENTS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' EMD_ITEM_PRICE_HISTORY
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy EMD Price History"
                    SQL = "SELECT * FROM EMD_ITEM_PRICE_HISTORY"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO EMD_ITEM_PRICE_HISTORY VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' EMD_UPDATE_HISTORY
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy EMD Update History"
                    SQL = "SELECT * FROM EMD_UPDATE_HISTORY"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO EMD_UPDATE_HISTORY VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' MARKET_HISTORY
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Market History"
                    SQL = "SELECT * FROM MARKET_HISTORY"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO MARKET_HISTORY VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' MARKET_HISTORY_UPDATE_CACHE
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Market History Cache"
                    SQL = "SELECT * FROM MARKET_HISTORY_UPDATE_CACHE"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO MARKET_HISTORY_UPDATE_CACHE VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' MARKET_ORDERS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Market Orders"
                    SQL = "SELECT * FROM MARKET_ORDERS"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO MARKET_ORDERS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' MARKET_ORDERS_UPDATE_CACHE
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Market Orders"
                    SQL = "SELECT * FROM MARKET_ORDERS_UPDATE_CACHE"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO MARKET_ORDERS_UPDATE_CACHE VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' PRICE_PROFILES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Price Profiles"
                    SQL = "SELECT * FROM PRICE_PROFILES"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        Call BeginSQLiteTransaction(DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO PRICE_PROFILES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' CREST_CACHE_DATES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy CREST_CACHE_DATES table"

                    ' See if they have the new fields for CREST updates first
                    SQL = "SELECT CREST_INDUSTRY_SPECIALIZATIONS_CACHED_UNTIL, CREST_INDUSTRY_TEAMS_CACHED_UNTIL, "
                    SQL = SQL & "CREST_INDUSTRY_TEAM_AUCTIONS_CACHED_UNTIL, CREST_INDUSTRY_SYSTEMS_CACHED_UNTIL, "
                    SQL = SQL & "CREST_INDUSTRY_FACILITIES_CACHED_UNTIL, CREST_MARKET_PRICES_CACHED_UNTIL FROM CREST_CACHE_DATES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    ' If it didn't error, they have the fields
                    If Err.Number = 0 Then
                        HaveNewEVEIPHFields = True
                    Else
                        HaveNewEVEIPHFields = False
                    End If

                    On Error GoTo 0

                    Call BeginSQLiteTransaction(DBNEW)

                    ' If they have the fields, then insert
                    If HaveNewEVEIPHFields Then
                        While readerUpdate.Read
                            SQL = "INSERT INTO CREST_CACHE_DATES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        readerUpdate.Close()
                        readerUpdate = Nothing
                        DBCommand = Nothing
                    Else
                        SQL = "INSERT INTO CREST_CACHE_DATES VALUES (NULL,NULL,NULL,NULL,NULL,NULL)"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)
                    End If

                    Call CommitSQLiteTransaction(DBNEW)

                    ' INDUSTRY_JOBS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy Industry Jobs"
                    SQL = "SELECT * FROM INDUSTRY_JOBS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then

                        ' See if they have the new or old format for industry jobs
                        On Error Resume Next
                        SQL = "SELECT successfulRuns FROM INDUSTRY_JOBS"
                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerCheck = DBCommand.ExecuteReader
                        ' If it didn't error, they have the field for the old table
                        If Err.Number = 0 Then
                            HaveNewIndustryJobsTable = True
                            readerCheck.Close()
                        Else
                            HaveNewIndustryJobsTable = False
                        End If
                        On Error GoTo 0

                        readerCheck = Nothing

                        Call BeginSQLiteTransaction(DBNEW)

                        ' If they have the new table, copy if the old then just leave it blank since the api will update
                        If HaveNewIndustryJobsTable Then
                            ' Copy the current data
                            While readerUpdate.Read
                                SQL = "INSERT INTO INDUSTRY_JOBS VALUES ("
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(18)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(19)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(20)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(21)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(22)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(23)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(24)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(25)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(26)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(27)) & ","
                                SQL = SQL & BuildInsertFieldString(CInt(readerUpdate.Item(28)))
                                SQL = SQL & ")"

                                DBCommand = New SQLiteCommand(SQL, DBNEW)
                                DBCommand.ExecuteNonQuery()
                                DBCommand = Nothing
                            End While
                        End If

                        Call CommitSQLiteTransaction(DBNEW)
                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' ITEM_PRICES - includes CREST Market Prices updates
                    ProgramErrorLocation = "Cannot copy Item Prices"
                    ' See if they have the new or old format for item prices
                    On Error Resume Next
                    SQL = "SELECT AVERAGE_PRICE FROM ITEM_PRICES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerCheck = DBCommand.ExecuteReader
                    ' If it didn't error
                    If Err.Number = 0 Then
                        HaveNewItemPricesFields = True
                        readerCheck.Close()
                    Else
                        HaveNewItemPricesFields = False
                    End If
                    On Error GoTo 0

                    Call BeginSQLiteTransaction(DBNEW)

                    If HaveNewItemPricesFields Then
                        SQL = "SELECT ITEM_ID, PRICE, ADJUSTED_PRICE, AVERAGE_PRICE FROM ITEM_PRICES"
                    Else
                        SQL = "SELECT [Item ID], [Price], 0 AS ADJUSTED_PRICE, 0 AS AVERAGE_PRICE FROM ITEM_PRICES"
                    End If

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    While readerUpdate.Read
                        SQL = "UPDATE ITEM_PRICES "
                        SQL = SQL & "SET PRICE = " & BuildInsertFieldString(readerUpdate.GetDouble(1)) & ", "
                        SQL = SQL & "ADJUSTED_PRICE = " & BuildInsertFieldString(readerUpdate.GetDouble(2)) & ", "
                        SQL = SQL & "AVERAGE_PRICE = " & BuildInsertFieldString(readerUpdate.GetDouble(3)) & " "
                        SQL = SQL & " WHERE ITEM_ID = " & BuildInsertFieldString(readerUpdate.GetValue(0))

                        DBCommand = New SQLiteCommand(SQL, DBNEW)
                        DBCommand.ExecuteNonQuery()
                        DBCommand = Nothing
                    End While

                    Call CommitSQLiteTransaction(DBNEW)
                    readerUpdate.Close()
                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' ITEM_PRICES_CACHE
                    ' See if they have the percentile values first
                    ProgramErrorLocation = "Cannot copy Item Price Cache"

                    On Error Resume Next
                    SQL = "SELECT allPercentile FROM ITEM_PRICES_CACHE"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerCheck = DBCommand.ExecuteReader
                    ' If it didn't error, they have the fields
                    If Err.Number = 0 Then
                        HavePrecentiles = True
                        readerCheck.Close()
                    Else
                        HavePrecentiles = False
                    End If
                    On Error GoTo 0

                    readerCheck = Nothing

                    Call BeginSQLiteTransaction(DBNEW)

                    If Not HavePrecentiles Then
                        SQL = "SELECT * FROM ITEM_PRICES_CACHE"
                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerUpdate = DBCommand.ExecuteReader

                        While readerUpdate.Read
                            SQL = "INSERT INTO ITEM_PRICES_CACHE VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & "0,"
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                            SQL = SQL & "0,"
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(18)) & ","
                            SQL = SQL & "0,"
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(19)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(20))
                            SQL = SQL & ")"

                            DBCommand = New SQLiteCommand(SQL, DBNEW)
                            DBCommand.ExecuteNonQuery()

                        End While
                        readerUpdate.Close()
                    Else
                        SQL = "SELECT * FROM ITEM_PRICES_CACHE"
                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerUpdate = DBCommand.ExecuteReader

                        While readerUpdate.Read
                            SQL = "INSERT INTO ITEM_PRICES_CACHE VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            If IsDBNull(readerUpdate.Item(7)) Then
                                SQL = SQL & "0,"
                            Else
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            End If
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                            If IsDBNull(readerUpdate.Item(7)) Then
                                SQL = SQL & "14,"
                            Else
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                            End If
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(18)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(19)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(20)) & ","
                            If IsDBNull(readerUpdate.Item(21)) Then
                                SQL = SQL & "21,"
                            Else
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(21)) & ","
                            End If
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(22)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(23))
                            SQL = SQL & ")"

                            DBCommand = New SQLiteCommand(SQL, DBNEW)
                            DBCommand.ExecuteNonQuery()

                        End While

                        readerUpdate.Close()

                    End If

                    Call CommitSQLiteTransaction(DBNEW)

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' OWNED_BLUEPRINTS
                    ProgramErrorLocation = "Cannot copy Owned Blueprints"
                    SQL = "SELECT * FROM OWNED_BLUEPRINTS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    readerCheck = Nothing

                    ' Copy all data over
                    Call BeginSQLiteTransaction(DBNEW)
                    While readerUpdate.Read
                        SQL = "INSERT INTO OWNED_BLUEPRINTS VALUES ("
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                        SQL = SQL & BuildInsertFieldString(CInt(readerUpdate.Item(7))) & ","
                        SQL = SQL & BuildInsertFieldString(CInt(readerUpdate.Item(8))) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                        SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14))
                        SQL = SQL & ")"

                        DBCommand = New SQLiteCommand(SQL, DBNEW)
                        DBCommand.ExecuteNonQuery()
                        DBCommand = Nothing
                    End While

                    Call CommitSQLiteTransaction(DBNEW)
                    readerUpdate.Close()
                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' STATIONS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy STATIONS Data table"
                    SQL = "SELECT * FROM STATIONS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        On Error Resume Next
                        Dim HaveReprocessingFields As Boolean
                        SQL = "SELECT REPROCESSING_TAX_RATE FROM STATIONS"
                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerCheck = DBCommand.ExecuteReader
                        ' If it didn't error, they have the fields
                        If Err.Number = 0 Then
                            HaveReprocessingFields = True
                            readerCheck.Close()
                        Else
                            HaveReprocessingFields = False
                        End If
                        On Error GoTo 0

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM STATIONS"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO STATIONS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            If HaveReprocessingFields Then
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7))
                            Else
                                SQL = SQL & "0,0"
                            End If
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' FW_SYSTEM_UPGRADES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy FW_SYSTEM_UPGRADES Data table"
                    SQL = "SELECT * FROM FW_SYSTEM_UPGRADES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Copy the table data
                        SQL = "SELECT * FROM FW_SYSTEM_UPGRADES"
                        DBCommand = New SQLiteCommand(SQL, DBOLD)
                        readerUpdate = DBCommand.ExecuteReader

                        While readerUpdate.Read
                            SQL = "INSERT INTO FW_SYSTEM_UPGRADES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' NEW CREST TABLES

                    ' INDUSTRY_CATEGORY_SPECIALTIES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_CATEGORY_SPECIALTIES Data table"
                    SQL = "SELECT * FROM INDUSTRY_CATEGORY_SPECIALTIES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_CATEGORY_SPECIALTIES"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_CATEGORY_SPECIALTIES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_FACILITIES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_FACILITIES Data table"
                    SQL = "SELECT * FROM INDUSTRY_FACILITIES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_FACILITIES"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_FACILITIES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_GROUP_SPECIALTIES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_GROUP_SPECIALTIES Data table"
                    SQL = "SELECT * FROM INDUSTRY_GROUP_SPECIALTIES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_GROUP_SPECIALTIES"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_GROUP_SPECIALTIES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_SYSTEMS_COST_INDICIES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_SYSTEMS_COST_INDICIES Data table"
                    SQL = "SELECT * FROM INDUSTRY_SYSTEMS_COST_INDICIES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the records first, then reload (if any)
                        SQL = "DELETE FROM INDUSTRY_SYSTEMS_COST_INDICIES"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_SYSTEMS_COST_INDICIES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_TEAMS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_TEAMS Data table"
                    SQL = "SELECT * FROM INDUSTRY_TEAMS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_TEAMS"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_TEAMS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_TEAMS_AUCTIONS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_TEAMS_AUCTIONS Data table"
                    SQL = "SELECT * FROM INDUSTRY_TEAMS_AUCTIONS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_TEAMS_AUCTIONS"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_TEAMS_AUCTIONS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_TEAMS_AUCTIONS_BIDS
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_TEAMS_AUCTIONS_BIDS Data table"
                    SQL = "SELECT * FROM INDUSTRY_TEAMS_AUCTIONS_BIDS"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_TEAMS_AUCTIONS_BIDS"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_TEAMS_AUCTIONS_BIDS VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' INDUSTRY_TEAMS_BONUSES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy INDUSTRY_TEAMS_BONUSES Data table"
                    SQL = "SELECT * FROM INDUSTRY_TEAMS_BONUSES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    On Error GoTo 0

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Delete all the station records first, then reload
                        SQL = "DELETE FROM INDUSTRY_TEAMS_BONUSES"
                        Call ExecuteNonQuerySQL(SQL, DBNEW)

                        While readerUpdate.Read
                            SQL = "INSERT INTO INDUSTRY_TEAMS_BONUSES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5))
                            SQL = SQL & ")"

                            Call ExecuteNonQuerySQL(SQL, DBNEW)

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()
                    End If

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    ' STATION_FACILITIES
                    On Error Resume Next
                    ProgramErrorLocation = "Cannot copy STATION_FACILITIES Data table"
                    SQL = "SELECT MATERIAL_MULTIPLIER FROM STATION_FACILITIES"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerCheck2 = DBCommand.ExecuteReader
                    readerCheck2.Read()

                    Dim Have6ModifierFields As Boolean
                    ' See if they have this change to make the multiplers into 1 field instead of 2
                    If Not IsNothing(readerCheck2) Then
                        Have6ModifierFields = False
                    Else
                        Have6ModifierFields = True
                    End If
                    readerCheck2.Close()
                    On Error GoTo 0

                    ProgramErrorLocation = "Cannot copy STATION_FACILITIES Data table"

                    SQL = " SELECT COUNT(*) FROM (SELECT DISTINCT FACILITY_ID, FACILITY_NAME, SOLAR_SYSTEM_ID, SOLAR_SYSTEM_NAME, SOLAR_SYSTEM_SECURITY, REGION_ID, REGION_NAME, "
                    SQL = SQL & "FACILITY_TYPE_ID, FACILITY_TYPE, ACTIVITY_ID, FACILITY_TAX, "
                    If Have6ModifierFields Then
                        SQL = SQL & "BASE_MM, BASE_TM, BASE_CM, ADDITIONAL_MM, ADDITIONAL_TM, ADDITIONAL_CM, GROUP_ID, CATEGORY_ID, COST_INDEX, OUTPOST "
                    Else
                        SQL = SQL & "MATERIAL_MULTIPLIER, TIME_MULTIPLIER, COST_MULTIPLIER, GROUP_ID, CATEGORY_ID, COST_INDEX, OUTPOST "
                    End If
                    SQL = SQL & "FROM STATION_FACILITIES WHERE OUTPOST = 1)"
                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader
                    readerUpdate.Read()
                    Count = readerUpdate.GetInt64(0)
                    readerUpdate.Close()

                    ' Copy all of the outpost data to the static station table - Make sure we dump any duplicates if they still exist
                    SQL = "SELECT DISTINCT FACILITY_ID, FACILITY_NAME, SOLAR_SYSTEM_ID, SOLAR_SYSTEM_NAME, SOLAR_SYSTEM_SECURITY, REGION_ID, REGION_NAME, "
                    SQL = SQL & "FACILITY_TYPE_ID, FACILITY_TYPE, ACTIVITY_ID, FACILITY_TAX, "
                    If Have6ModifierFields Then
                        SQL = SQL & "BASE_MM, BASE_TM, BASE_CM, ADDITIONAL_MM, ADDITIONAL_TM, ADDITIONAL_CM, GROUP_ID, CATEGORY_ID, COST_INDEX, OUTPOST "
                    Else
                        SQL = SQL & "MATERIAL_MULTIPLIER, TIME_MULTIPLIER, COST_MULTIPLIER, GROUP_ID, CATEGORY_ID, COST_INDEX, OUTPOST "
                    End If
                    SQL = SQL & "FROM STATION_FACILITIES WHERE OUTPOST = 1"

                    DBCommand = New SQLiteCommand(SQL, DBOLD)
                    readerUpdate = DBCommand.ExecuteReader

                    Me.Invoke(UpdateStatusDelegate, True, "Updating Station Data...")
                    worker.ReportProgress(Counter)

                    ' They might not have this table yet.
                    If Not IsNothing(readerUpdate) Then
                        ' They have the new table, but need to import all the outpost data they have
                        Call BeginSQLiteTransaction(DBNEW)

                        ' Insert all the outpost data
                        While readerUpdate.Read
                            ' Insert
                            SQL = "INSERT INTO STATION_FACILITIES VALUES ("
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(0)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(1)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(2)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(3)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(4)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(5)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(6)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(7)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(8)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(9)) & ","
                            SQL = SQL & BuildInsertFieldString(readerUpdate.Item(10)) & ","
                            If Have6ModifierFields Then
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11) * readerUpdate.Item(14)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12) * readerUpdate.Item(15)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13) * readerUpdate.Item(16)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(18)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(19)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(20)) & ")"
                            Else
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(11)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(12)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(13)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(14)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(15)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(16)) & ","
                                SQL = SQL & BuildInsertFieldString(readerUpdate.Item(17)) & ")"
                            End If


                            Call ExecuteNonQuerySQL(SQL, DBNEW)
                            Counter += 1

                            On Error Resume Next
                            worker.ReportProgress(CInt(Counter / Count * 100))
                            On Error GoTo 0

                        End While

                        Call CommitSQLiteTransaction(DBNEW)

                        readerUpdate.Close()

                    End If

                    Call BeginSQLiteTransaction(DBNEW)

                    ' Need to update the station_facilities table with all the old indicies so they don't load a new db with nothing (only outposts above)
                    SQL = "SELECT DISTINCT SOLAR_SYSTEM_ID, ACTIVITY_ID, COST_INDEX FROM INDUSTRY_SYSTEMS_COST_INDICIES"
                    DBCommand = New SQLiteCommand(SQL, DBNEW)
                    readerUpdate = DBCommand.ExecuteReader

                    While readerUpdate.Read
                        SQL = "UPDATE STATION_FACILITIES SET COST_INDEX = " & CStr(readerUpdate.GetDouble(2)) & " "
                        SQL = SQL & " WHERE SOLAR_SYSTEM_ID = " & CStr(readerUpdate.GetInt64(0)) & " AND ACTIVITY_ID = " & CStr(readerUpdate.GetInt32(1))
                        Call ExecuteNonQuerySQL(SQL, DBNEW)
                    End While

                    readerUpdate.Close()
                    Call CommitSQLiteTransaction(DBNEW)

                    readerUpdate = Nothing
                    DBCommand = Nothing

                    DBOLD.Close()
                    DBNEW.Close()
                    DBNEW = Nothing
                    DBOLD = Nothing

                    Me.Invoke(UpdateStatusDelegate, False, "Database Updated...")
                    ProgramErrorLocation = ""
                    SQL = ""
                End If

            ElseIf UpdateFileList(i).Name = EVE_IMAGES_ZIP Then
                Me.Invoke(UpdateStatusDelegate, False, "Installing Image Updates...")
                ProgramErrorLocation = "Cannot copy images"

                Using zip = New ZipFile(UPDATES_FOLDER & UpdateFileList(i).Name)
                    EVEImagesNewLocalFolderName = EVE_IMAGES_ZIP.Substring(0, Len(EVE_IMAGES_ZIP) - 4) ' Save as base name
                    zip.ExtractAll(UPDATES_FOLDER & EVEImagesNewLocalFolderName)
                End Using

                ProgramErrorLocation = ""
                SQL = ""
            End If
        Next

        ProgramErrorLocation = ""
        SQL = ""
        Application.DoEvents()

        ' If we screw up after this, we have to revert to anything we changed if possible
        On Error GoTo RevertToOldFileVersions

        ' Rename all files/folders with OLD and copy over new files/folders
        RecordCount = UpdateFileList.Count - 1

        For i = 0 To RecordCount
            Me.Invoke(UpdateStatusDelegate, False, "Copying Files...")
            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit Sub
            Else
                ' Report progress.
                If RecordCount > 0 Then
                    worker.ReportProgress((i / RecordCount) + 1 * 10)
                End If
            End If

            If UpdateFileList(i).Name = EVE_IMAGES_ZIP Then
                Me.Invoke(UpdateStatusDelegate, False, "Updating Images...")

                ' Delete OLD folder if it exists
                If Directory.Exists(ROOT_FOLDER & OLD_PREFIX & EVEImagesLocalFolderName) Then
                    ProgramErrorLocation = "Error Deleting Old Images"
                    ' Delete what is there and replace
                    Directory.Delete(ROOT_FOLDER & OLD_PREFIX & EVEImagesLocalFolderName, True)
                    Application.DoEvents()
                End If

                ' Rename the current folder to old
                If Directory.Exists(ROOT_FOLDER & EVEImagesLocalFolderName) Then
                    ProgramErrorLocation = "Error Moving Old Images"
                    Directory.Move(ROOT_FOLDER & EVEImagesLocalFolderName, ROOT_FOLDER & OLD_PREFIX & EVEImagesLocalFolderName)
                    Application.DoEvents()
                End If

                ' Move the new image folder from updates folder to root directory folder
                ProgramErrorLocation = "Error Moving New Images"
                Directory.Move(UPDATES_FOLDER & EVEImagesNewLocalFolderName, ROOT_FOLDER & EVEImagesNewLocalFolderName)
                Application.DoEvents()

            ElseIf UpdateFileList(i).Name = EVE_DB Then
                Me.Invoke(UpdateStatusDelegate, False, "Updating DB...")

                ' If an OLD file exists, delete it
                If File.Exists(ROOT_FOLDER & OLD_PREFIX & EVE_DB) Then
                    ProgramErrorLocation = "Error Deleting Old Database"
                    File.Delete(ROOT_FOLDER & OLD_PREFIX & EVE_DB)
                    Application.DoEvents()
                End If

                ' Rename old file if it exists to old prefix
                If File.Exists(ROOT_FOLDER & EVE_DB) Then
                    ProgramErrorLocation = "Error Moving Old Database"
                    File.Move(ROOT_FOLDER & EVE_DB, ROOT_FOLDER & OLD_PREFIX & EVE_DB)
                    Application.DoEvents()
                End If

                ' Move new file
                ProgramErrorLocation = "Error Moving New Database"
                File.Move(UPDATES_FOLDER & EVE_DB, ROOT_FOLDER & EVE_DB)
                Application.DoEvents()

            Else
                Me.Invoke(UpdateStatusDelegate, False, "Updating " & UpdateFileList(i).Name & "...")

                ' If an OLD file exists, delete it
                If File.Exists(ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name) Then
                    ProgramErrorLocation = "Error Deleting Old " & UpdateFileList(i).Name & "file"
                    File.Delete(ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name)
                    Application.DoEvents()
                End If

                ' Rename old file if it exists to old prefix
                If File.Exists(ROOT_FOLDER & UpdateFileList(i).Name) Then
                    ProgramErrorLocation = "Error Moving Old " & UpdateFileList(i).Name & "file"
                    File.Move(ROOT_FOLDER & UpdateFileList(i).Name, ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name)
                    Application.DoEvents()
                End If

                ' Move new file
                ProgramErrorLocation = "Error Moving New " & UpdateFileList(i).Name & "file"
                File.Move(UPDATES_FOLDER & UpdateFileList(i).Name, ROOT_FOLDER & UpdateFileList(i).Name)
                Application.DoEvents()

            End If
        Next

        ProgramErrorLocation = ""
        Me.Invoke(UpdateStatusDelegate, False, "Cleaning up Temp Files...")
        DBCommand = Nothing

        Exit Sub

RevertToOldFileVersions:

        ' Output error first
        Call WriteMsgToLog(Err.Description)

        On Error Resume Next

        ' If we get here, try to delete everything we downloaded and rename any files saved as "Old" to original names
        ProgramErrorLocation = ProgramErrorLocation & " - Reverted to Old file versions"
        ' Save the error
        ThrownError = Err.Description

        ' Delete the updates folder
        If Directory.Exists(UPDATES_FOLDER) Then
            ' Delete what is there and replace
            Directory.Delete(UPDATES_FOLDER, True)
            Application.DoEvents()
        End If

        ' Rename all files/folders 
        If Not IsNothing(UpdateFileList) Then
            For i = 0 To UpdateFileList.Count - 1

                If UpdateFileList(i).Name = EVE_IMAGES_ZIP Then
                    ' Delete the new folder if the old one renamed
                    If Directory.Exists(ROOT_FOLDER & OLD_PREFIX & EVEImagesNewLocalFolderName) Then
                        ' Delete it
                        Directory.Delete(ROOT_FOLDER & OLD_PREFIX & EVEImagesNewLocalFolderName, True)
                        Application.DoEvents()
                    End If

                    ' Rename the old zip folder
                    Directory.Move(ROOT_FOLDER & OLD_PREFIX & EVEImagesLocalFolderName, ROOT_FOLDER & EVEImagesLocalFolderName)
                    Application.DoEvents()

                ElseIf UpdateFileList(i).Name = EVE_DB Then

                    ' If an OLD file exists, delete new, rename old
                    If File.Exists(ROOT_FOLDER & OLD_PREFIX & EVE_DB) Then
                        File.Delete(ROOT_FOLDER & EVE_DB)
                        Application.DoEvents()
                        File.Move(ROOT_FOLDER & OLD_PREFIX & EVE_DB, ROOT_FOLDER & EVE_DB)
                        Application.DoEvents()
                    End If

                Else
                    ' Only rename if old version exists
                    If File.Exists(ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name) Then
                        ' Delete the new file
                        File.Delete(ROOT_FOLDER & UpdateFileList(i).Name)
                        Application.DoEvents()
                        ' Rename old file back 
                        File.Move(ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name, ROOT_FOLDER & UpdateFileList(i).Name)
                        Application.DoEvents()
                    End If
                End If

            Next
        End If

        Exit Sub

    End Sub

    ' This event handler updates the progress.
    Private Sub BGWorker_ProgressChanged(ByVal sender As System.Object, ByVal e As ProgressChangedEventArgs) Handles BGWorker.ProgressChanged

        Dim safedelegate As New UpdatePGBarSafe(AddressOf UpdateProgressBar)
        Me.Invoke(safedelegate, e.ProgressPercentage) 'Invoke the TreadsafeDelegate

    End Sub

    ' Shows message box with message sent
    Private Sub ShowNotifyBox(LabelText As String)
        Dim f1 As New frmNotify
        f1.lblNotify.Text = LabelText
        f1.ShowDialog()
    End Sub

    ' This event handler deals with the results of the background operation.
    Private Sub BGWorker_RunWorkerCompleted(ByVal sender As System.Object, ByVal e As RunWorkerCompletedEventArgs) Handles BGWorker.RunWorkerCompleted
        Dim ErrorText As String = ""

        On Error Resume Next

        ' Allow the messagebox to pop up over the form now
        Me.TopMost = False

        ErrorText = e.Error.ToString

        ' Clean up all OLD files and folders that might be left around
        Call CleanUpOLDFiles()

        Application.UseWaitCursor = False

        If e.Cancelled = True Then
            lblUpdateMain.Text = "Update Canceled"
            Call ShowNotifyBox("Update Canceled")
        ElseIf (e.Error IsNot Nothing) Or (ProgramErrorLocation <> "") Then

            lblUpdateMain.Text = "Update Failed."

            ' Write sql and error to log
            If SQL <> "" Then
                ErrorText = ErrorText & " SQL: " & SQL
            End If

            Call WriteMsgToLog(ErrorText)

            Dim MainMessage As String = "There was an error in the update. Program not updated."

            If ThrownError <> "" And ProgramErrorLocation <> "" Then
                MsgBox(MainMessage & vbCrLf & ProgramErrorLocation & vbCrLf & "Error: " & ThrownError, vbCritical, Application.ProductName)
            ElseIf ProgramErrorLocation <> "" Then
                MsgBox(MainMessage & vbCrLf & ProgramErrorLocation, vbCritical, Application.ProductName)
            ElseIf ThrownError <> "" Then
                MsgBox(MainMessage & vbCrLf & "Error: " & ThrownError, vbCritical, Application.ProductName)
            Else
                Call ShowNotifyBox(MainMessage)
            End If

        Else
            Me.Hide()
            lblUpdateMain.Text = "Update Complete."
            ' We have completed the update
            ' Copy over the old XML file and delete the old
            If File.Exists(ROOT_FOLDER & LocalXMLFileName) Then
                File.Delete(ROOT_FOLDER & LocalXMLFileName)
            End If

            File.Move(UPDATES_FOLDER & LocalXMLFileName, ROOT_FOLDER & LocalXMLFileName)

            ' Finally delete the updates folder
            Directory.Delete(UPDATES_FOLDER, True)

            ' Wait for a second before running - might solve the problem with incorrectly suggesting an update
            Thread.Sleep(1000)

            Call ShowNotifyBox("Update Complete!")
        End If

        ' Shell to program
        Dim ProcInfo As New ProcessStartInfo
        ProcInfo.FileName = EVEIPH_SHELL_PATH
        ProcInfo.UseShellExecute = True
        ProcInfo.WindowStyle = ProcessWindowStyle.Normal

        Process.Start(EVEIPH_SHELL_PATH)

        On Error Resume Next
        ' Done
        End

    End Sub

    Private Sub CleanUpOLDFiles()
        Dim ImageDir As DirectoryInfo

        On Error Resume Next

        For i = 0 To UpdateFileList.Count - 1

            If UpdateFileList(i).Name = EVE_IMAGES_ZIP Then
                ' Downloaded old folder
                Directory.Delete(ROOT_FOLDER & OLD_PREFIX & EVEImagesLocalFolderName, True)
            ElseIf UpdateFileList(i).Name = EVE_DB Then
                ' Delete old file
                File.Delete(ROOT_FOLDER & OLD_PREFIX & EVE_DB)
            Else
                ' Delete old file
                File.Delete(ROOT_FOLDER & OLD_PREFIX & UpdateFileList(i).Name)
            End If

        Next
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        If BGWorker.WorkerSupportsCancellation = True Then
            ' Cancel the asynchronous operation.
            BGWorker.CancelAsync()
        End If
    End Sub

    ' When the form is shown, run the updates
    Private Sub frmUpdaterMain_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        Me.Refresh()
        Application.DoEvents()
        Application.UseWaitCursor = True

        If BGWorker.IsBusy <> True Then
            ' Start the asynchronous operation.
            BGWorker.RunWorkerAsync()
        End If

        Me.Refresh()
        Application.DoEvents()
        Application.UseWaitCursor = False

    End Sub

    ' Downloads the sent file from server and saves it to the root directory as the sent file name
    Public Function DownloadFileFromServer(ByVal DownloadURL As String, ByVal FileName As String) As String
        'Creating the request and getting the response
        Dim Response As HttpWebResponse
        Dim Request As HttpWebRequest

        ' File sizes for progress bar
        Dim FileSize As Double

        ' For reading in chunks of data
        Dim readBytes(4095) As Byte
        ' Save in root directory
        Dim writeStream As New FileStream(FileName, FileMode.Create)
        Dim bytesread As Integer

        'Replacement for Stream.Position (webResponse stream doesn't support seek)
        Dim nRead As Long

        worker.ReportProgress(0)

        Try 'Checks if the file exist
            Request = DirectCast(HttpWebRequest.Create(DownloadURL), HttpWebRequest)
            'Request.Proxy = GetProxyData()
            Request.Credentials = CredentialCache.DefaultCredentials ' Added 9/27 to attempt to fix error: (407) Proxy Authentication Required.
            Request.Timeout = 50000
            Response = CType(Request.GetResponse, HttpWebResponse)
        Catch ex As Exception
            ' Set as empty and return
            writeStream.Close()
            Return ""
        End Try

        ' Get size
        FileSize = Response.ContentLength()

        ' Loop through and get the file in chunks, save out
        Do
            Application.DoEvents()

            If worker.CancellationPending Then 'If user abort download
                Exit Do
            End If

            bytesread = Response.GetResponseStream.Read(readBytes, 0, 4096)

            ' No more bytes to read
            If bytesread = 0 Then Exit Do

            nRead = nRead + bytesread
            ' Update progress 
            worker.ReportProgress((nRead * 100) / FileSize)

            writeStream.Write(readBytes, 0, bytesread)
        Loop

        'Close the streams
        Response.GetResponseStream.Close()
        writeStream.Close()

        Return FileName

    End Function

    ' MD5 Hash - specify the path to a file and this routine will calculate your hash
    Public Function MD5CalcFile(ByVal filepath As String) As String

        ' open file (as read-only)
        If File.Exists(filepath) Then
            Using reader As New System.IO.FileStream(filepath, IO.FileMode.Open, IO.FileAccess.Read)
                Using md5 As New System.Security.Cryptography.MD5CryptoServiceProvider

                    ' hash contents of this stream
                    Dim hash() As Byte = md5.ComputeHash(reader)

                    ' return formatted hash
                    Return ByteArrayToString(hash)

                End Using
            End Using
        End If

        ' Something went wrong
        Return ""

    End Function

    ' MD5 Hash - utility function to convert a byte array into a hex string
    Private Function ByteArrayToString(ByVal arrInput() As Byte) As String

        Dim sb As New System.Text.StringBuilder(arrInput.Length * 2)

        For i As Integer = 0 To arrInput.Length - 1
            sb.Append(arrInput(i).ToString("X2"))
        Next

        Return sb.ToString().ToLower

    End Function

    ' Formats the value sent to what we want to insert inot the table field
    Private Function BuildInsertFieldString(ByVal inValue As Object) As String
        Dim CheckNullValue As Object
        Dim OutputString As String

        ' See if it is null first
        CheckNullValue = CheckNull(inValue)

        If CStr(CheckNullValue) <> "null" Then
            ' Not null, so format
            If inValue.GetType.Name <> "String" Then
                ' Just a value, so no quotes needed
                OutputString = CStr(inValue)
            Else
                ' String, so check for appostrophes and add quotes
                OutputString = "'" & CheckString(CStr(inValue)) & "'"
            End If
        Else
            OutputString = "null"
        End If

        Return OutputString

    End Function

    Public Function CheckNull(ByVal inVariable As Object) As Object
        If IsNothing(inVariable) Then
            Return "null"
        ElseIf DBNull.Value.Equals(inVariable) Then
            Return "null"
        Else
            Return inVariable
        End If
    End Function

    Public Sub ExecuteNonQuerySQL(ByVal SQL As String, ByRef db As SQLiteConnection)
        Dim DBExecuteCmd As SQLiteCommand

        DBExecuteCmd = New SQLiteCommand(SQL, db)
        DBExecuteCmd.ExecuteNonQuery()

        DBExecuteCmd.Dispose()
    End Sub

    Public Sub BeginSQLiteTransaction(ByRef DB As SQLiteConnection)
        Call ExecuteNonQuerySQL("BEGIN;", DB)
    End Sub

    Public Sub CommitSQLiteTransaction(ByRef DB As SQLiteConnection)
        Call ExecuteNonQuerySQL("END;", DB)
    End Sub

    Public Function CheckString(ByVal inStrVar As String) As String
        ' Anything with quote mark in name it won't correctly load - need to replace with double quotes
        If InStr(inStrVar, "'") <> 0 Then
            inStrVar = Replace(inStrVar, "'", "''")
        End If
        Return inStrVar
    End Function

    ' Writes a sent message to a log file
    Public Sub WriteMsgToLog(ByVal ErrorMsg As String)
        Dim FilePath As String = "EVEIPH.log"
        Dim AllText() As String

        ' Only write to log if there is an error to write
        If Trim(ErrorMsg) <> "" Then
            If Not IO.File.Exists(FilePath) Then
                Dim sw As IO.StreamWriter = IO.File.CreateText(FilePath)
                sw.Close()
            End If

            ' This is an easier way to get all of the strings in the file.
            AllText = IO.File.ReadAllLines(FilePath)
            ' This will append the string to the end of the file.
            My.Computer.FileSystem.WriteAllText(FilePath, CStr(Now) & " - " & ErrorMsg & vbCrLf, True)
        End If

    End Sub

    Public Function GetProxyData() As WebProxy
        Dim ReturnProxy As WebProxy

        Dim ProxyAddress As String = CStr(GetSettingValue(AppSettingsFileName, SettingTypes.TypeString, AppSettingsFileName, "ProxyAddress", DefaultProxyAddress))
        Dim ProxyPort As Integer = CInt(GetSettingValue(AppSettingsFileName, SettingTypes.TypeInteger, AppSettingsFileName, "ProxyPort", DefaultProxyPort))

        If ProxyAddress <> "" Then
            If ProxyPort <> 0 Then
                ReturnProxy = New WebProxy(ProxyAddress, ProxyPort)
            Else
                ReturnProxy = New WebProxy(ProxyAddress)
            End If

            ReturnProxy = New WebProxy(ProxyAddress, ProxyPort)
            ReturnProxy.Credentials = CredentialCache.DefaultCredentials

            Return ReturnProxy
        Else
            Return Nothing
        End If

    End Function

    ' Gets a value from a referenced XML file by searching for it
    Private Function GetSettingValue(ByRef FileName As String, ObjectType As SettingTypes, RootElement As String, ElementString As String, DefaultValue As Object) As Object
        Dim m_xmld As New XmlDocument
        Dim m_nodelist As XmlNodeList

        Dim TempValue As String

        'Load the Xml file
        m_xmld.Load(SettingsFolder & FileName & XMLfileType)

        'Get the settings

        ' Get the cache update
        m_nodelist = m_xmld.SelectNodes("/" & RootElement & "/" & ElementString)

        If Not IsNothing(m_nodelist.Item(0)) Then
            ' Should only be one
            TempValue = m_nodelist.Item(0).InnerText

            ' If blank, then return default
            If TempValue = "" Then
                Return DefaultValue
            End If

            If TempValue = "False" Or TempValue = "True" Then
                ' Change to type boolean
                ObjectType = SettingTypes.TypeBoolean
            End If

            ' Found it, return the cast
            Select Case ObjectType
                Case SettingTypes.TypeBoolean
                    Return CBool(TempValue)
                Case SettingTypes.TypeDouble
                    Return CDbl(TempValue)
                Case SettingTypes.TypeInteger
                    Return CInt(TempValue)
                Case SettingTypes.TypeString
                    Return CStr(TempValue)
                Case SettingTypes.TypeLong
                    Return CLng(TempValue)
            End Select

        Else
            ' Doesn't exist, use default
            Return DefaultValue
        End If

        Return Nothing

    End Function

End Class