#include "displaytext.h"

#include <QtGlobal>
#include <QApplication>
#include <QMouseEvent>
#include <QTextCharFormat>
#include <QFont>
#include <QTextCursor>

#include "Configuration.hpp"
#include "qt_helpers.hpp"

#include "moc_displaytext.cpp"

DisplayText::DisplayText(QWidget *parent) :
    QTextEdit(parent)
{
    setReadOnly (true);
    viewport ()->setCursor (Qt::ArrowCursor);
    setWordWrapMode (QTextOption::NoWrap);
    setStyleSheet ("");
    // max lines to limit heap usage
    document ()->setMaximumBlockCount (10000);
}

void DisplayText::setConfiguration(Configuration const * config)
{
  scroll_ = config->scroll();
  useDarkStyle_ = config->useDarkStyle();
  displayCountryName_ = config->countryName();
  displayCountryPrefix_ = config->countryPrefix();
  displayNewCQZ_ = config->newCQZ();
  displayNewCQZBand_ = config->newCQZBand();
  displayNewCQZBandMode_ = config->newCQZBandMode();
  displayNewITUZ_ = config->newITUZ();
  displayNewITUZBand_ = config->newITUZBand();
  displayNewITUZBandMode_ = config->newITUZBandMode();
  displayNewDXCC_ = config->newDXCC();
  displayNewDXCCBand_ = config->newDXCCBand();
  displayNewDXCCBandMode_ = config->newDXCCBandMode();
  displayNewGrid_ = config->newGrid();
  displayNewGridBand_ = config->newGridBand();
  displayNewGridBandMode_ = config->newGridBandMode();
  displayNewPx_ = config->newPx();
  displayNewPxBand_ = config->newPxBand();
  displayNewPxBandMode_ = config->newPxBandMode();
  displayNewCall_ = config->newCall();
  displayNewCallBand_ = config->newCallBand();
  displayNewCallBandMode_ = config->newCallBandMode();
  displayPotential_ = config->newPotential();
  displayTxtColor_ = config->txtColor();
  displayWorkedColor_ = config->workedColor();
  displayWorkedStriked_ = config->workedStriked();
  displayWorkedUnderlined_ = config->workedUnderlined();
  displayWorkedDontShow_ = config->workedDontShow();
  beepOnNewCQZ_ = config->beepOnNewCQZ();
  beepOnNewITUZ_ = config->beepOnNewITUZ();
  beepOnNewDXCC_ = config->beepOnNewDXCC();
  beepOnNewGrid_ = config->beepOnNewGrid();
  beepOnNewPx_ = config->beepOnNewPx();
  beepOnNewCall_ = config->beepOnNewCall();
  beepOnMyCall_ = config->beepOnMyCall();
  RR73Marker_ = config->RR73Marker();
  otherMessagesMarker_ = config->otherMessagesMarker();
  enableCountryFilter_ = config->enableCountryFilter();
  enableCallsignFilter_ = config->enableCallsignFilter();
  hidefree_ = config->hidefree();
  enableMyConinentFilter_ = config->hideOwnContinent();
  showcq_ = config->showcq();
  showcqrrr73_ = config->showcqrrr73();
  showcq73_ = config->showcq73();
  redMarker_ = config->redMarker();
  blueMarker_ = config->blueMarker();
  hidehintMarker_ = config->hidehintMarker();
  hide_TX_messages_ = config->hide_TX_messages();
  color_MyCall_ = config->color_MyCall().name();
  color_CQ_ = config->color_CQ().name();
  color_StandardCall_ = config->color_StandardCall().name();
  color_WorkedCall_ = config->color_WorkedCall().name();
  color_NewCQZ_ = config->color_NewCQZ().name();
  color_NewCQZBand_ = config->color_NewCQZBand().name();
  color_NewITUZ_ = config->color_NewITUZ().name();
  color_NewITUZBand_ = config->color_NewITUZBand().name();
  color_NewDXCC_ = config->color_NewDXCC().name();
  color_NewDXCCBand_ = config->color_NewDXCCBand().name();
  color_NewGrid_ = config->color_NewGrid().name();
  color_NewGridBand_ = config->color_NewGridBand().name();
  color_NewPx_ = config->color_NewPx().name();
  color_NewPxBand_ = config->color_NewPxBand().name();
  color_NewCall_ = config->color_NewCall().name();
  color_NewCallBand_ = config->color_NewCallBand().name();
  hideContinents_ = config->hideContinents();
  countries_ = config->countries();
  callsigns_ = config->callsigns();
  myCall_ = config->my_callsign();   
}

void DisplayText::setMyContinent(QString const& mycontinet)
{
    myContinent_ = mycontinet;
}

void DisplayText::setContentFont(QFont const& font)
{
//  setFont (font);
  m_charFormat.setFont (font);
  bold_ = font.bold();
  m_charFormat.setFontItalic(false);
//  selectAll ();
/*
  auto cursor = textCursor ();
  cursor.select(QTextCursor::Document);
  cursor.mergeCharFormat (m_charFormat);
  cursor.clearSelection ();
  cursor.movePosition (QTextCursor::End);

  // position so viewport scrolled to left
  cursor.movePosition (QTextCursor::Up);
  cursor.movePosition (QTextCursor::StartOfLine);

  setTextCursor (cursor);
  ensureCursorVisible (); */
}

void DisplayText::mouseDoubleClickEvent(QMouseEvent *e)
{
  bool ctrl = (e->modifiers() & Qt::ControlModifier);
  bool alt = (e->modifiers() & Qt::AltModifier);
  QTextEdit::mouseDoubleClickEvent(e);
  emit(selectCallsign(alt,ctrl));
}

void DisplayText::insertLineSpacer(QString const& line)
{
    appendText (line, Radio::convert_dark("#d3d3d3",useDarkStyle_), Radio::convert_dark("#000000",useDarkStyle_), 0, " ", Radio::convert_dark("#000000",useDarkStyle_), " ", true);
}

void DisplayText::appendText(QString const& text, QString const& bg, QString const& color, int std_type, QString const& servis, QString const& servis_color, QString const& cntry, bool forceBold, bool strikethrough, bool underlined, bool DXped, bool overwrite, bool wanted)
{
    QString servbg, s;
    if (std_type == 2) servbg = Radio::convert_dark("#ff0000",useDarkStyle_);
    else if (std_type == 5) servbg = Radio::convert_dark("#0000ff",useDarkStyle_);
    else if (servis == "?") servbg = Radio::convert_dark("#ffff00",useDarkStyle_);
    else if (std_type == 3 && servis.length()>1) servbg = servis.mid(1);
    else servbg = Radio::convert_dark("#ffffff",useDarkStyle_);
    auto cursor = textCursor ();
    if (scroll_) {
        if (document ()->blockCount() == 10000) {
            cursor.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, 9998);
            cursor.select(QTextCursor::LineUnderCursor);
            cursor.removeSelectedText();
            cursor.deleteChar();
        }
        cursor.movePosition (QTextCursor::Start);
        if (overwrite) {
            cursor.select(QTextCursor::LineUnderCursor);
            cursor.removeSelectedText();
        }
    } else {
        cursor.movePosition (QTextCursor::End);
        if (0 == cursor.position ())
            cursor.setCharFormat (m_charFormat);
        else if (overwrite) {
            cursor.select(QTextCursor::LineUnderCursor);
            cursor.removeSelectedText();
        } else
            cursor.insertText ("\n");
    }    
    if (forceBold) {
        if (bold_) { 
            m_charFormat.setFontWeight(QFont::Black);
        } else {
            m_charFormat.setFontWeight(QFont::Bold);
        }
    } else {
        if (bold_) { 
            m_charFormat.setFontWeight(QFont::Bold);
        } else {
            m_charFormat.setFontWeight(QFont::Normal);
        }
    }
    m_charFormat.setForeground(QColor(color));
    m_charFormat.setBackground (QColor(bg));
    if (text.length() < 50) {
        int ft = 23; 
        if (text.mid(4,1) == " ") ft = 21;
        cursor.insertText (text.left(ft),m_charFormat);
        m_charFormat.setFontStrikeOut(strikethrough);
        if (DXped) {
            if (underlined)  m_charFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            else  m_charFormat.setUnderlineStyle(QTextCharFormat::DashUnderline);
        } 
        else if (underlined) m_charFormat.setUnderlineStyle(QTextCharFormat::SingleUnderline);
        else m_charFormat.setUnderlineStyle(QTextCharFormat::NoUnderline);
        if (wanted) {
            m_charFormat.setFontItalic(true); m_charFormat.setForeground(QColor(color_MyCall_)); m_charFormat.setFontOverline(true);
            if (!underlined && !DXped) m_charFormat.setFontUnderline(true);
            }
        cursor.insertText (text.mid(ft,26),m_charFormat);
        if (wanted) {
            m_charFormat.setFontItalic(false); m_charFormat.setFontOverline(false);
            if (!underlined && !DXped) m_charFormat.setFontUnderline(false);
            }
        m_charFormat.setFontStrikeOut(false);
        m_charFormat.setUnderlineStyle(QTextCharFormat::NoUnderline);
        m_charFormat.setBackground (QColor(servbg));
        m_charFormat.setForeground(QColor(servis_color));
        cursor.insertText (servis.left(1),m_charFormat);
        m_charFormat.setBackground (QColor(Radio::convert_dark("#ffffff",useDarkStyle_)));
        m_charFormat.setForeground(QColor(Radio::convert_dark("#000000",useDarkStyle_)));
        cursor.insertText (cntry,m_charFormat);
    } else {
        cursor.insertText (text.trimmed(),m_charFormat);
    }
    if (scroll_ && !overwrite) cursor.insertText ("\n");
    else cursor.movePosition (QTextCursor::StartOfLine);
    setTextCursor (cursor);
    ensureCursorVisible ();
    document ()->setMaximumBlockCount (document ()->maximumBlockCount ());
}

int DisplayText::displayDecodedText(DecodedText* decodedText, QString myCall, QString hisCall, QString hisGrid,
                            bool once_notified, LogBook logBook, QsoHistory& qsoHistory,
                            QsoHistory& qsoHistory2, double dialFreq, const QString app_mode,
                            bool bypassRxfFilters,bool bypassAllFilters, int rx_frq,
                            QStringList wantedCallList, QStringList wantedPrefixList, QStringList wantedGridList, 
                            QStringList wantedCountryList, bool windowPopup, QWidget* window)
{
    QString bgColor = Radio::convert_dark("#ffffff",useDarkStyle_);
    QString txtColor = Radio::convert_dark("#000000",useDarkStyle_);
    QString swpColor = "";
    QString servisColor = Radio::convert_dark("#000000",useDarkStyle_);
    QString messageText;
    bool forceBold = false;
    bool strikethrough = false;
    bool underlined = false;
    bool beep = false;
    bool actwind = false;
    bool show_line = true;
    bool jt65bc = false;
    bool notified = false;
    bool new_marker = false;
    int inotified = 0;
    int std_type = 0;
    bool bwantedCall = false;
    bool bwantedPrefix = false;
    bool bwantedGrid = false;
    bool bwantedCountry = false;
    if (app_mode.startsWith("FT")) messageText = decodedText->string().left(49);
    else if (app_mode == "WSPR-2") messageText = decodedText->string().trimmed();
    else messageText = decodedText->string().left(40);
    QString servis = " ";
    QString cntry = " ";
    QString checkCall;
    QString checkCall2;
    QString grid;
    QString tyyp="";
    QString countryName;
    QString countryName2;
    QString mpx="";
    QString lotw="";
    QsoHistory::Status status = QsoHistory::NONE;
    QsoHistory::Status dummy = QsoHistory::NONE;
    int priority = 0;
    QString param;
    QString report;
    QString checkMode;
    QString rep_type;
    unsigned c_time = 0;
    if (!decodedText->isDebug() && app_mode != "WSPR-2") {
        c_time = decodedText->timeInSeconds();
        if (c_time != 0 && c_time != max_r_time) {
            max_r_time = c_time;
            qsoHistory.time(max_r_time);
            if (!hisCall.isEmpty ()) {
                mystatus_ = qsoHistory2.status(hisCall,mygrid_);
                if (mygrid_.isEmpty ()) mygrid_ = hisGrid;
                myhisCall_ = hisCall;
                }
        }
        QStringList parts = decodedText->message().split (' ', SkipEmptyParts);
        if (!hisCall.isEmpty () && messageText.contains(Radio::base_callsign (hisCall))) txtColor = color_StandardCall_;
        checkCall = decodedText->CQersCall(grid,tyyp);
        checkCall2 = decodedText->call();
        if(!app_mode.startsWith("FT") && (messageText.contains("2nd-h") || messageText.contains("3rd-h"))) jt65bc = true;
        if (!checkCall.isEmpty ()) {
            if (grid.isEmpty ()) dummy = qsoHistory2.status(checkCall,grid);
            if (grid.isEmpty () && Radio::base_callsign (checkCall) == hisCall) grid = hisGrid;
            if (decodedText->message().left(3) == "DE "){
                tyyp = "";
                if (qAbs(rx_frq - decodedText->frequencyOffset()) < 10 ) {
                    std_type = 2;
                    txtColor = color_MyCall_;
                     
                    if (!grid.isEmpty () && grid != "RR73" && hisCall.isEmpty ()) {
                        status = QsoHistory::RCALL;
                        param = grid;
                    }
                    else if (hisCall.isEmpty () && decodedText->message().right(4) == checkCall) {
                        status = QsoHistory::RCALL;
                    }
                    else if (checkCall.contains(hisCall)  && mystatus_ > QsoHistory::SCQ  && mystatus_ != QsoHistory::FIN) {
                        if (decodedText->report(myCall,Radio::base_callsign (checkCall),report,rep_type) && !report.isEmpty ()) {
                            if (rep_type == "R")
                                status = QsoHistory::RRREPORT;
                            else
                                status = QsoHistory::RREPORT;
                            param = report;
                        }
                        else if (decodedText->message().contains(" RRR") && mystatus_ > QsoHistory::SREPORT) {
                            status = QsoHistory::RRR;
                        }
                        else if (decodedText->message().contains("RR73") && mystatus_ > QsoHistory::SREPORT) {
                            status = QsoHistory::RRR73;
                        }
                        else if (decodedText->message().contains(" 73") && mystatus_ >= QsoHistory::RRR && mystatus_ != QsoHistory::FIN) { // DE call 73 case
                            status = QsoHistory::R73;
                        }
                        else {
                            std_type = 3;
                            txtColor = Radio::convert_dark("#000000",useDarkStyle_);
                        }
                    } else {
                        std_type = 3;
                        txtColor = Radio::convert_dark("#000000",useDarkStyle_);
                    }
                } else {
                    std_type = 3;
                }
            } else {
                std_type = 1;
                txtColor = color_CQ_;
                status = QsoHistory::RCQ;
                param = grid;
            }
        }
        else if (!myCall.isEmpty () && Radio::base_callsign (checkCall2) == myCall) {
                std_type = 2;
                txtColor = color_MyCall_;
                actwind = true;
                if (beepOnMyCall_) {
                    beep = true;
                }
                decodedText->deCallAndGrid(checkCall, grid);
                if (!grid.isEmpty () || (!checkCall.isEmpty () && parts.length() == 2)) {
                    status = QsoHistory::RCALL;
                    if (grid.isEmpty ()) dummy = qsoHistory2.status(checkCall,grid);
                    if (grid.isEmpty () && Radio::base_callsign (checkCall) == hisCall) grid = hisGrid;
                    param = grid;
                }
                else {
                    if (grid.isEmpty ()) dummy = qsoHistory2.status(checkCall,grid);
                    if (grid.isEmpty () && Radio::base_callsign (checkCall) == hisCall) grid = hisGrid;
                    if (decodedText->report(myCall,Radio::base_callsign (checkCall),report,rep_type)) {
                        if (!checkCall.isEmpty ()) {
                            if (!report.isEmpty ()) {
                                if (rep_type == "R")
                                    status = QsoHistory::RRREPORT;
                                else
                                    status = QsoHistory::RREPORT;
                                param = report;
                            }
                            else if (decodedText->message().contains(" RRR")) {
                                status = QsoHistory::RRR;
                            }
                            else if (decodedText->message().contains("RR73")) {
                                status = QsoHistory::RRR73;
                            }
                            else if (decodedText->message().contains(" 73")) {
                                status = QsoHistory::R73;
                            }
                            else {
                                status = QsoHistory::RCALL;
                            }
                        }
                    }            
                    else if (decodedText->message().contains("73") && !hisCall.isEmpty () && mystatus_ >= QsoHistory::RRR && mystatus_ != QsoHistory::FIN) { // nonstandard73 with myCall
                        status = QsoHistory::R73;
                        checkCall = hisCall;
                    }
                    if (!checkCall.isEmpty ()) {
                        if (hisCall.isEmpty () && (myhisCall_.isEmpty () || !checkCall.contains(myhisCall_))) {
                            mystatus_ = qsoHistory2.status(Radio::base_callsign (checkCall),mygrid_);
                            myhisCall_ = Radio::base_callsign (checkCall);
                        }
                        if ((!hisCall.isEmpty () && checkCall.contains(hisCall)) || (!myhisCall_.isEmpty () && checkCall.contains(myhisCall_))) {
                            mystatus_ = status;
                            if (grid.isEmpty () && !mygrid_.isEmpty ()) {
                                grid = mygrid_;
                            }
                        }
                    }
                }
        }
        else {
                decodedText->deCallAndGrid(checkCall, grid);
                if (!checkCall.isEmpty ()) {
                    if (grid.isEmpty ()) dummy = qsoHistory2.status(checkCall,grid);
                    if (grid.isEmpty () && Radio::base_callsign (checkCall) == hisCall) grid = hisGrid;
                    if (!decodedText->isNonStd1() && !decodedText->isNonStd2()) { 
                        std_type = 3;
                        if (!grid.isEmpty ()) param = grid;
                        if (!hisCall.isEmpty () && checkCall.contains(hisCall)) qsoHistory.rx(checkCall,decodedText->frequencyOffset());
                    } else if (!hisCall.isEmpty () && checkCall.contains(hisCall) && qAbs(rx_frq - decodedText->frequencyOffset()) < 10 && decodedText->message().contains("73") && mystatus_ >= QsoHistory::RRR && mystatus_ != QsoHistory::FIN) { //nonstandard73 with hisCall
                        std_type = 2;
                        txtColor = color_MyCall_;
                        status = QsoHistory::R73;
                        mystatus_ = status;
                    } else if (!hisCall.isEmpty () && checkCall.contains(hisCall)) {
                        qsoHistory.rx(checkCall,decodedText->frequencyOffset());
                        checkCall = "";
                    } else if (!decodedText->isNonStd2()) {
                        std_type = 3;
                        if (!grid.isEmpty ()) param = grid;
                        if (!hisCall.isEmpty () && checkCall.contains(hisCall)) qsoHistory.rx(checkCall,decodedText->frequencyOffset());
                    } else {
                        checkCall = "";
                    }
                    if (!checkCall.isEmpty () && RR73Marker_ && (decodedText->message().contains("RR73") || decodedText->message().contains(" 73"))) {
                        std_type = 4;
                        txtColor = color_CQ_;
                        status = QsoHistory::RFIN;
                    }
                } else if (std_type == 0 && !hisCall.isEmpty () && qAbs(rx_frq - decodedText->frequencyOffset()) < 10 && decodedText->message().contains("73") && mystatus_ >= QsoHistory::RRR && mystatus_ != QsoHistory::FIN) { // nonstandard 73 in my rx
                    std_type = 2;
                    txtColor = color_MyCall_;
                    checkCall = hisCall;
                    status = QsoHistory::R73;
                    mystatus_ = status;
                }
        }
    } else checkCall = "";
    if (!checkCall.isEmpty ()) {

        bool cqzB4 = true;
        bool ituzB4 = true;
        bool countryB4 = true;
        bool pxB4 = true;
        bool callB4 = true;
        bool cqzB4BandMode = true;
        bool ituzB4BandMode = true;
        bool countryB4BandMode = true;
        bool pxB4BandMode = true;
        bool callB4BandMode = true;
        bool gridB4 = true;
        bool gridB4BandMode = true;
        logBook.getLOTW(/*in*/ checkCall, /*out*/ lotw);
        if (!lotw.isEmpty ()) {
            priority = 1;
        }
        if (displayPotential_ && std_type == 3) {
            txtColor = color_StandardCall_;
        }
        if (app_mode == "JT9+JT65") {
            if (decodedText->isJT9()) {
                checkMode = "JT9";
            } else if (decodedText->isJT65()) { // TODO: is this if-condition necessary?
                checkMode = "JT65";
            }
        } else {
            checkMode = app_mode;
        }
        if (!jt65bc && (displayCountryName_ || displayNewCQZ_ || displayNewITUZ_ || displayNewDXCC_ || displayNewCall_ || displayNewGrid_ || displayNewPx_)) {
            if (!displayNewCQZ_ && !displayNewITUZ_ && !displayNewDXCC_ && displayCountryName_ && !displayNewCall_ && !displayNewPx_) {
                        logBook.getDXCC(/*in*/ checkCall, /*out*/ countryName);
                    }
            if (displayNewCQZ_) {
                if (displayNewCQZBand_ || displayNewCQZBandMode_) {
                    if (displayNewCQZBand_ && displayNewCQZBandMode_) {
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewCQZBand_){
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchCQZ(/*in*/ checkCall, /*out*/ countryName, cqzB4 ,cqzB4BandMode);
                }
            }
            if (displayNewITUZ_) {
                if (displayNewITUZBand_ || displayNewITUZBandMode_) {
                    if (displayNewITUZBand_ && displayNewITUZBandMode_) {
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewITUZBand_){
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchITUZ(/*in*/ checkCall, /*out*/ countryName, ituzB4 ,ituzB4BandMode);
                }
            }
            if (displayNewDXCC_) {
                if (displayNewDXCCBand_ || displayNewDXCCBandMode_) {
                    if (displayNewDXCCBand_ && displayNewDXCCBandMode_) {
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewDXCCBand_){
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchDXCC(/*in*/ checkCall, /*out*/ countryName, countryB4 ,countryB4BandMode);
                }
            }
            if (displayNewGrid_) {
                if (displayNewGridBand_ || displayNewGridBandMode_) {
                    if (displayNewGridBand_ && displayNewGridBandMode_) {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewGridBand_) {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchGrid(/*in*/ grid.trimmed(), /*out*/ gridB4 ,gridB4BandMode);
                }
            }
            if (displayNewPx_) {
                if (displayNewPxBand_ || displayNewPxBandMode_) {
                    if (displayNewPxBand_ && displayNewPxBandMode_) {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewPxBand_) {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchPX(/*in*/ checkCall, /*out*/ countryName, pxB4 ,pxB4BandMode);
                }
            }
            if (displayNewCall_) {
                if (displayNewCallBand_ || displayNewCallBandMode_) {
                    if (displayNewCallBand_ && displayNewCallBandMode_) {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewCallBand_) {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchCall(/*in*/ checkCall, /*out*/ countryName, callB4 ,callB4BandMode);
                }
            }

            if (displayNewCQZ_ || displayNewITUZ_ || displayNewDXCC_ || displayNewCall_ || displayNewGrid_ || displayNewPx_) {
//Worked
                if ((displayPotential_ && std_type == 3) || (std_type != 3)) {
                    if (displayWorkedColor_) {
                        bgColor = color_WorkedCall_;
                    }
                    if (displayWorkedStriked_) {
                        strikethrough = true;
                    } else if (displayWorkedUnderlined_) {
                        underlined = true;
                    }
                } else if (displayWorkedColor_ && otherMessagesMarker_) servis += color_WorkedCall_;

                if (displayNewCQZ_ && !cqzB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewCQZ_;
                        if (!lotw.isEmpty ()) priority = 31;
                        else priority = 30;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCQZ_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewCQZ_;
                        if (!lotw.isEmpty ()) priority = 31;
                        else priority = 30;
                        new_marker = true;
                    }
                } else if ((displayNewCQZBand_ || displayNewCQZBandMode_) && !cqzB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewCQZBand_;
                        if (!lotw.isEmpty ()) priority = 29;
                        else priority = 28;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCQZ_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewCQZBand_;
                        if (!lotw.isEmpty ()) priority = 29;
                        else priority = 28;
                        new_marker = true;
                    }
                } else if (displayNewITUZ_ && !ituzB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewITUZ_;
                        if (!lotw.isEmpty ()) priority = 27;
                        else priority = 26;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewITUZ_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewITUZ_;
                        if (!lotw.isEmpty ()) priority = 27;
                        else priority = 26;
                        new_marker = true;
                    }
                } else if ((displayNewITUZBand_ || displayNewITUZBandMode_) && !ituzB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewITUZBand_;
                        if (!lotw.isEmpty ()) priority = 25;
                        else priority = 24;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewITUZ_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewITUZBand_;
                        if (!lotw.isEmpty ()) priority = 25;
                        else priority = 24;
                        new_marker = true;
                    }
                } else if (displayNewDXCC_ && !countryB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewDXCC_;
                        if (!lotw.isEmpty ()) priority = 23;
                        else priority = 22;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewDXCC_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewDXCC_;
                        if (!lotw.isEmpty ()) priority = 23;
                        else priority = 22;
                        new_marker = true;
                    }
                } else if ((displayNewDXCCBand_ || displayNewDXCCBandMode_) && !countryB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewDXCCBand_;
                        if (!lotw.isEmpty ()) priority = 21;
                        else priority = 20;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewDXCC_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewDXCCBand_;
                        if (!lotw.isEmpty ()) priority = 21;
                        else priority = 20;
                        new_marker = true;
                    }
                } else if (displayNewGrid_ && !gridB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewGrid_;
                        if (!lotw.isEmpty ()) priority = 16;
                        else priority = 15;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewGrid_) {
                            beep = true;
                        }
                    }
                    else  if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewGrid_;
                        if (!lotw.isEmpty ()) priority = 16;
                        else priority = 15;
                        new_marker = true;
                    }
                } else if ((displayNewGridBand_ || displayNewGridBandMode_) && !gridB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewGridBand_;
                        if (!lotw.isEmpty ()) priority = 14;
                        else priority = 13;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewGrid_) {
                            beep = true;
                        }
                    }
                    else if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewGridBand_;
                        if (!lotw.isEmpty ()) priority = 14;
                        else priority = 13;
                        new_marker = true;
                    }
                } else  if (displayNewPx_ && !pxB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewPx_;
                        if (!lotw.isEmpty ()) priority = 12;
                        else priority = 11;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewPx_) {
                            beep = true;
                        }
                    }
                    else  if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewPx_;
                        if (!lotw.isEmpty ()) priority = 12;
                        else priority = 11;
                        new_marker = true;
                    }
                } else if ((displayNewPxBand_ || displayNewPxBandMode_) && !pxB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewPxBand_;
                        if (!lotw.isEmpty ()) priority = 10;
                        else priority = 9;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewPx_) {
                            beep = true;
                        }
                    }
                    else  if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewPxBand_;
                        if (!lotw.isEmpty ()) priority = 10;
                        else priority = 9;
                        new_marker = true;
                    }
                } else  if (displayNewCall_ && !callB4) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewCall_;
                        if (!lotw.isEmpty ()) priority = 8;
                        else priority = 7;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCall_) {
                            beep = true;
                        }
                    }
                    else  if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewCall_;
                        if (!lotw.isEmpty ()) priority = 8;
                        else priority = 7;
                        new_marker = true;
                    }
                } else if ((displayNewCallBand_ || displayNewCallBandMode_) && !callB4BandMode) {
                    if ((displayPotential_ && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = color_NewCallBand_;
                        if (!lotw.isEmpty ()) priority = 6;
                        else priority = 5;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCall_) {
                            beep = true;
                        }
                    }
                    else  if (otherMessagesMarker_) {
                        servis = servis.left(1) + color_NewCallBand_;
                        if (!lotw.isEmpty ()) priority = 6;
                        else priority = 5;
                        new_marker = true;
                    }
                } 
                if (displayWorkedDontShow_ && std_type != 2 && ((!forceBold && ((displayPotential_ && std_type == 3) || std_type != 3)) || (!new_marker && otherMessagesMarker_ && std_type == 3))) {
                    show_line = false;
                }
            }
        } else {
            logBook.getDXCC(/*in*/ checkCall, /*out*/ countryName);
        }

        QStringList items = countryName.split(',');
        mpx = items[1];

        if (!wantedCallList.isEmpty() && (wantedCallList.indexOf(Radio::base_callsign (checkCall)) >= 0 || wantedCallList.indexOf(checkCall) >= 0)) {
            bwantedCall = true; show_line = true;
        }
        for (int i=0; i<wantedPrefixList.size(); i++) {
            if (wantedPrefixList.at(i).size() > 1 && checkCall.startsWith(wantedPrefixList.at(i))) {
                bwantedPrefix = true;
                break;
            }
        }
        for (int i=0; i<wantedGridList.size(); i++) {
            if (wantedGridList.at(i).size() > 0 && wantedGridList.at(i).left(4) == grid.left(4)) {
                bwantedGrid = true;
                break;
            }
        }
        for (int i=0; i<wantedCountryList.size(); i++) {
            if (wantedCountryList.at(i).size() > 0 && wantedCountryList.at(i) == mpx.toUpper()) {
                bwantedCountry = true;
                break;
            }
        }
        if (bwantedCall && priority < 5) {
            priority = 4;
            beep = true;
        } else if ((bwantedPrefix || bwantedGrid) && priority < 5) {
            priority = 3;
//            beep = true;
        } else if (bwantedCountry && priority < 5) {
            priority = 2;
//            beep = true;
        } else if (bwantedCall && priority < 20) {
            priority = 19;
            beep = true;
        } else if ((bwantedPrefix || bwantedGrid) && priority < 20) {
            priority = 18;
            beep = true;
        } else if (bwantedCountry && priority < 20) {
            priority = 17;
            beep = true;
        }
         
            
        if (displayTxtColor_ && (displayPotential_ || std_type != 3)) {
            swpColor = bgColor;
            bgColor = txtColor;
            txtColor = swpColor;
        }
        if (displayCountryName_) {
            if (displayCountryPrefix_) {
                cntry = items[1];
                
            } else {
                // do some obvious abbreviations, don't care if we using just prefixes here, not big deal to run some replace's
                cntry = items[2];
            }
        }
        if (!bwantedCall && !bwantedPrefix && !bwantedGrid && !bwantedCountry) {
            if (hideContinents_.contains(items[0]) && std_type != 2 && !jt65bc) {
                show_line = false;
            } else if (enableCountryFilter_ && std_type != 2 && !jt65bc) {
                QStringList countries = countries_.split(',');
                if (countries.contains(items[1].toUpper()))
                    show_line = false;
            }
            if (show_line && enableCallsignFilter_ && std_type != 2 && !jt65bc) {
                QStringList callsigns = callsigns_.split(',');
                if (callsigns.contains(Radio::base_callsign (checkCall)))
                    show_line = false;
            }
        }
        else if (!bwantedCall && enableCallsignFilter_ && std_type != 2 && !jt65bc) {
            QStringList callsigns = callsigns_.split(',');
            if (callsigns.contains(Radio::base_callsign (checkCall)))
                show_line = false;
        }
        if (enableMyConinentFilter_ && std_type != 2 && !jt65bc) {
            logBook.getDXCC(/*in*/ checkCall2, /*out*/ countryName2);
            QString continent2 =  countryName2.split(',')[0];
            if (continent2 == "  ") continent2 = myContinent_;
            if ((std_type == 1 && myContinent_ == items[0]) || (continent2 == myContinent_ && (items[0] == myContinent_ || items[0] == "  ")) || (continent2 != myContinent_ && items[0] != myContinent_)) show_line = false;
        }
    } else if (enableMyConinentFilter_ && !jt65bc) {
        logBook.getDXCC(/*in*/ checkCall2, /*out*/ countryName2);
        QString continent2 =  countryName2.split(',')[0];
        if (continent2 == "  ") continent2 = myContinent_;
        if (continent2 == myContinent_) show_line = false;
    }
    
    if (show_line && decodedText->isNonStd2() && hidefree_ && !decodedText->message().contains(myCall) && std_type != 1 && !jt65bc) {
        show_line = false;
    } else if (show_line && showcq_ && std_type != 1 && std_type != 2 && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (show_line && showcqrrr73_ && std_type != 1 && std_type != 2 && !decodedText->isEnd() && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (show_line && showcq73_ && std_type != 1 && std_type != 2 && !decodedText->isFin() && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (!hidehintMarker_ && decodedText->isHint()) {
        if(lotw.isEmpty ())
            servis = "*" + servis.mid(1); // hinted decode
        else
            servis = "°" + servis.mid(1); // lotw hinted decode
    } else if (decodedText->isWrong()) {
        servis = "?" + servis.mid(1); // error decode
    } else if (!lotw.isEmpty ()) {
        servis = "•" + servis.mid(1); // lotw 
    }
    if (bypassAllFilters || bypassRxfFilters) {
            show_line = true;
    }
    if (show_line) {
        if (actwind) {
            if (windowPopup && window != NULL) {
                window->showNormal();
				window->raise();
				QApplication::setActiveWindow(window);
			}
		}
        if (beep && !once_notified) {
            QApplication::beep();
			notified = true;
        }
    }
    if (jt65bc) {
        bgColor = Radio::convert_dark("#ffffff",useDarkStyle_);
        txtColor = Radio::convert_dark("#000000",useDarkStyle_);
    }
    if (show_line) {
        if (!checkCall.isEmpty () && (std_type == 1 || std_type == 2 || std_type == 4 || (std_type == 3 && !param.isEmpty()))) {
            qsoHistory.message(checkCall,status,priority,param,tyyp,countryName.left(2),mpx,c_time,decodedText->report(),decodedText->frequencyOffset(),checkMode);
        } 
        if (std_type == 2) {
            if(!redMarker_) std_type = 0;
            else if(blueMarker_ && !hisCall.isEmpty () && checkCall.contains(hisCall)) std_type = 5;
        }
        appendText(messageText, bgColor, txtColor, std_type, servis, servisColor, cntry, forceBold, strikethrough, underlined, decodedText->isDXped(), false, bwantedCall||bwantedGrid||bwantedPrefix||bwantedCountry);
        wastx_ = false;
    }
        if (notified) inotified |= 1;
        if (show_line) inotified |= 2;
        if (bwantedCall) inotified |= 8;
        if (bwantedPrefix) inotified |= 16;
        if (bwantedGrid) inotified |= 32;
        if (bwantedCountry) inotified |= 64;
	return inotified;
}


void DisplayText::displayTransmittedText(QString text, QString myCall, QString hisCall, QString skip_tx1, QString modeTx, qint32 txFreq,
                                         QColor color_TxMsg, QsoHistory& qsoHistory)
{
    QsoHistory::Status status = QsoHistory::S73;
    QString bg=color_TxMsg.name();
    QString t;
    QString t1=" @ ";
    QString t2;
    QString tyyp = "";
    unsigned ttime=0;
    t = text;
    int dxped = t.indexOf("; ");
    if (dxped >0) {
        auto next_ws = t.indexOf(' ',dxped+2);
        t2 = t.mid(next_ws,t.indexOf(' ',next_ws+1)-next_ws);
        next_ws = t.indexOf(' ',1);
        t =  t.left(next_ws) + t2 + t.mid(next_ws,32);
    }
    t2 = QString::asprintf("%4d",txFreq);
    if(modeTx=="FT8") t1=" ~ ";
    else if(modeTx=="FT4") t1=" : ";
    else if(modeTx=="JT65") t1=" # ";
    else if(modeTx=="T10") t1=" + ";
    
    QStringList txs = t.split ("; ");
    for (int i=0; i<txs.size(); i++) {
        if(modeTx.startsWith("FT")) {
          t = QDateTime::currentDateTimeUtc().toString("hhmmss") + \
            "  Tx      " + t2 + t1 + txs.at(i).left(24);
          t = t.leftJustified(49,' ');
        } else {
          t = QDateTime::currentDateTimeUtc().toString("hhmm") + \
            "  Tx      " + t2 + t1 + txs.at(i).left(19);
        }
        ttime = 3600 * t.mid (0, 2).toUInt () + 60 * t.mid (2, 2).toUInt();
        if (t.mid (4, 2) != "  ") ttime += t.mid (4, 2).toUInt();        

        QStringList parts = txs.at(i).split (' ', SkipEmptyParts);
        if (parts.size () > 1) 
          {
            QString param="";
            QString call=parts[0];
            if (call == "DE ") call = hisCall;
            if (parts.size () > 2)
              {
                if (parts[0] == "CQ")
                  {
                    status = QsoHistory::SCQ;
                    if (parts.size () > 3) {
                        tyyp = parts [1];
                        if (tyyp == "908") tyyp = "JA";
                        call=parts[2];
                        param=parts[3];
                    } else {
                        call=parts[1];
                        param=parts[2];
                    }
                  }            
                else if (parts[2].size() == 4)
                  {
                    if (parts[2] == "RR73")
                      status = QsoHistory::SRR73;
                    else if (parts[2].left(2) == "R+" || parts[2].left(2) == "R-") {
                      param = parts[2].mid(1,3);
                      status = QsoHistory::SRREPORT;
                      tyyp = skip_tx1;
                    }
                    else
                      status = QsoHistory::SCALL;
                  }
                else if (parts[2].size() == 3)
                  {
                    if (parts[2] == "RRR")
                      status = QsoHistory::SRR;
                    else if (parts[2].left(1) == "+" || parts[2].left(1) == "-") {
                      param = parts[2].left(3);
                      status = QsoHistory::SREPORT;
                      tyyp = skip_tx1;
                     }
                   }
                else if (parts[2] == "73" || parts[1] == "73" || parts[0] == "73"
                      || parts[2] == "TNX" || parts[1] == "TNX" || parts[0] == "TNX"
                      || parts[2] == "TKS" || parts[1] == "TKS" || parts[0] == "TKS"
                      || parts[2] == "TU" || parts[1] == "TU" || parts[0] == "TU"
                      || (parts.size() == 4 && (parts[3] == "73" || parts[3] == "TNX" || parts[3] == "TKS" || parts[3] == "TU")))
                   {
                    call = hisCall;
                     status = QsoHistory::S73;
                   }
              }  
            if (parts.size () == 2)
               {
                if (parts[0] == "CQ")
                  {
                    status = QsoHistory::SCQ;
                    call=parts[1];
                  }            
                else if (!myCall.isEmpty () && Radio::base_callsign (parts[1]) == myCall && call != "73" && call != "TNX" && call != "TKS" && call != "TU")  // 
                  {
                    status = QsoHistory::SCALL;
                  }
                else if (call == "73" || parts[1] == "73" || call == "TNX" || parts[1] == "TNX" || call == "TKS" || parts[1] == "TKS" || call == "TU" || parts[1] == "TU")
                  {
                    call = hisCall;
                    status = QsoHistory::S73;
                  }
               }
            mystatus_ = status;
            qsoHistory.message(call,status,0,param,tyyp,"","",ttime,"",txFreq,modeTx);
          }
        if (wastx_ && ttime - last_tx < 2 && hide_TX_messages_)
            appendText(t,bg,Radio::convert_dark("#000000",useDarkStyle_),0," ",Radio::convert_dark("#000000",useDarkStyle_)," ",false,false,false,false,true);
        else
            appendText(t,bg,Radio::convert_dark("#000000",useDarkStyle_),0," ",Radio::convert_dark("#000000",useDarkStyle_));
    }
    wastx_ = true;
    last_tx = ttime;
}

void DisplayText::displayQSY(QString text)
{
  QString t = QDateTime::currentDateTimeUtc().toString("hhmmss") + "            " + text;
  QString bg=Radio::convert_dark("#ff69b4",useDarkStyle_);
  appendText(t,bg,Radio::convert_dark("#000000",useDarkStyle_),0," ",Radio::convert_dark("#000000",useDarkStyle_));
}

