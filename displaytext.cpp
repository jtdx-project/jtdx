
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
}

void DisplayText::setConfiguration(Configuration const * config)
{
  m_config = config;
}
void DisplayText::setContentFont(QFont const& font)
{
//  setFont (font);
  m_charFormat.setFont (font);
  bold_ = font.bold();
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
    appendText (line, "#d3d3d3", "#000000", 0, " ", " ", true);
}

void DisplayText::appendText(QString const& text, QString const& bg, QString const& color, int std_type, QString const& servis, QString const& cntry, bool forceBold, bool strikethrough, bool underlined, bool DXped, bool overwrite)
{
    QString servbg, s;
    if (std_type == 2) servbg = "#ff0000";
    else if (std_type == 5) servbg = "#0000ff";
    else if (servis == "?") servbg = "#ffff00";
    else if (std_type == 3 && servis.length()>1) servbg = servis.mid(1);
    else servbg = "#ffffff";
    auto cursor = textCursor ();
    cursor.movePosition (QTextCursor::End);
    if (0 == cursor.position ())
        cursor.setCharFormat (m_charFormat);
    else if (overwrite) {
        cursor.select(QTextCursor::LineUnderCursor);
        cursor.removeSelectedText();
    } else
        cursor.insertText ("\n");
    
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
    m_charFormat.setForeground(QColor(Radio::convert_dark(color,useDarkStyle_)));
    m_charFormat.setBackground (QColor(Radio::convert_dark(bg,useDarkStyle_)));
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
        cursor.insertText (text.mid(ft,26),m_charFormat);
        m_charFormat.setFontStrikeOut(false);
        m_charFormat.setUnderlineStyle(QTextCharFormat::NoUnderline);
        m_charFormat.setBackground (QColor(Radio::convert_dark(servbg,useDarkStyle_)));
        m_charFormat.setForeground(QColor(Radio::convert_dark("#000000",useDarkStyle_)));
        cursor.insertText (servis.left(1),m_charFormat);
        m_charFormat.setBackground (QColor(Radio::convert_dark("#ffffff",useDarkStyle_)));
        cursor.insertText (cntry,m_charFormat);
    } else {
        cursor.insertText (text.trimmed(),m_charFormat);
    }
    cursor.movePosition (QTextCursor::StartOfLine);
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
    QString bgColor = "#ffffff";
    QString txtColor = "#000000";
    QString swpColor = "";
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
    useDarkStyle_ = m_config->useDarkStyle();
    bool displayCountryName = m_config->countryName();
    bool displayCountryPrefix = m_config->countryPrefix();

    bool displayNewCQZ = m_config->newCQZ();
    bool displayNewCQZBand = m_config->newCQZBand();
    bool displayNewCQZBandMode = m_config->newCQZBandMode();
    bool displayNewITUZ = m_config->newITUZ();
    bool displayNewITUZBand = m_config->newITUZBand();
    bool displayNewITUZBandMode = m_config->newITUZBandMode();
    bool displayNewDXCC = m_config->newDXCC();
    bool displayNewDXCCBand = m_config->newDXCCBand();
    bool displayNewDXCCBandMode = m_config->newDXCCBandMode();
    bool displayNewGrid = m_config->newGrid();
    bool displayNewGridBand = m_config->newGridBand();
    bool displayNewGridBandMode = m_config->newGridBandMode();
    bool displayNewPx = m_config->newPx();
    bool displayNewPxBand = m_config->newPxBand();
    bool displayNewPxBandMode = m_config->newPxBandMode();
    bool displayNewCall = m_config->newCall();
    bool displayNewCallBand = m_config->newCallBand();
    bool displayNewCallBandMode = m_config->newCallBandMode();
    bool displayPotential = m_config->newPotential();
    bool displayTxtColor = m_config->txtColor();
    bool displayWorkedColor = m_config->workedColor();
    bool displayWorkedStriked = m_config->workedStriked();
    bool displayWorkedUnderlined = m_config->workedUnderlined();
    bool displayWorkedDontShow = m_config->workedDontShow();
    bool beepOnNewCQZ = m_config->beepOnNewCQZ();
    bool beepOnNewITUZ = m_config->beepOnNewITUZ();
    bool beepOnNewDXCC = m_config->beepOnNewDXCC();
    bool beepOnNewGrid = m_config->beepOnNewGrid();
    bool beepOnNewPx = m_config->beepOnNewPx();
    bool beepOnNewCall = m_config->beepOnNewCall();
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
    QString grid;
    QString tyyp="";
    QString countryName;
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
        auto const& parts = decodedText->message().split (' ', QString::SkipEmptyParts);
        checkCall = decodedText->CQersCall(grid,tyyp);
        if(!app_mode.startsWith("FT") && (messageText.contains("2nd-h") || messageText.contains("3rd-h"))) jt65bc = true;
        if (!checkCall.isEmpty ()) {
            if (grid.isEmpty ()) dummy = qsoHistory2.status(checkCall,grid);
            if (grid.isEmpty () && Radio::base_callsign (checkCall) == hisCall) grid = hisGrid;
            if (decodedText->message().left(3) == "DE "){
                tyyp = "";
                if (qAbs(rx_frq - decodedText->frequencyOffset()) < 10 ) {
                    std_type = 2;
                    txtColor = m_config->color_MyCall().name();
                     
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
                            txtColor = "#000000";
                        }
                    } else {
                        std_type = 3;
                        txtColor = "#000000";
                    }
                } else {
                    std_type = 3;
                }
            } else {
                std_type = 1;
                txtColor = m_config->color_CQ().name();
                status = QsoHistory::RCQ;
                param = grid;
            }
        }
        else if (!myCall.isEmpty () && Radio::base_callsign (decodedText->call()) == myCall) {
                std_type = 2;
                txtColor = m_config->color_MyCall().name();
                actwind = true;
                if (m_config->beepOnMyCall()) {
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
                        txtColor = m_config->color_MyCall().name();
                        status = QsoHistory::R73;
                        mystatus_ = status;
                    } else if (!hisCall.isEmpty () && checkCall.contains(hisCall)) {
                        qsoHistory.rx(checkCall,decodedText->frequencyOffset());
                        checkCall = "";
                    } else {
                        checkCall = "";
                    }
                    if (!checkCall.isEmpty () && m_config->RR73Marker() && (decodedText->message().contains("RR73") || decodedText->message().contains(" 73"))) {
                        std_type = 4;
                        txtColor = m_config->color_CQ().name();
                        status = QsoHistory::RFIN;
                    }
                } else if (std_type == 0 && !hisCall.isEmpty () && qAbs(rx_frq - decodedText->frequencyOffset()) < 10 && decodedText->message().contains("73") && mystatus_ >= QsoHistory::RRR && mystatus_ != QsoHistory::FIN) { // nonstandard 73 in my rx
                    std_type = 2;
                    txtColor = m_config->color_MyCall().name();
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
        if (!lotw.isEmpty ()) priority = 1;
        if (displayPotential && std_type == 3) {
            txtColor = m_config->color_StandardCall().name();
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
        if (!jt65bc && (displayCountryName || displayNewCQZ || displayNewITUZ || displayNewDXCC || displayNewCall || displayNewGrid || displayNewPx)) {
            if (!displayNewCQZ && !displayNewITUZ && !displayNewDXCC && displayCountryName && !displayNewCall && !displayNewPx) {
                        logBook.getDXCC(/*in*/ checkCall, /*out*/ countryName);
                    }
            if (displayNewCQZ) {
                if (displayNewCQZBand || displayNewCQZBandMode) {
                    if (displayNewCQZBand && displayNewCQZBandMode) {
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewCQZBand){
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchCQZ(/*in*/checkCall,/*out*/countryName,cqzB4,cqzB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchCQZ(/*in*/ checkCall, /*out*/ countryName, cqzB4 ,cqzB4BandMode);
                }
            }
            if (displayNewITUZ) {
                if (displayNewITUZBand || displayNewITUZBandMode) {
                    if (displayNewITUZBand && displayNewITUZBandMode) {
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewITUZBand){
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchITUZ(/*in*/checkCall,/*out*/countryName,ituzB4,ituzB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchITUZ(/*in*/ checkCall, /*out*/ countryName, ituzB4 ,ituzB4BandMode);
                }
            }
            if (displayNewDXCC) {
                if (displayNewDXCCBand || displayNewDXCCBandMode) {
                    if (displayNewDXCCBand && displayNewDXCCBandMode) {
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewDXCCBand){
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchDXCC(/*in*/checkCall,/*out*/countryName,countryB4,countryB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchDXCC(/*in*/ checkCall, /*out*/ countryName, countryB4 ,countryB4BandMode);
                }
            }
            if (displayNewGrid) {
                if (displayNewGridBand || displayNewGridBandMode) {
                    if (displayNewGridBand && displayNewGridBandMode) {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewGridBand) {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchGrid(/*in*/grid.trimmed(),/*out*/gridB4,gridB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchGrid(/*in*/ grid.trimmed(), /*out*/ gridB4 ,gridB4BandMode);
                }
            }
            if (displayNewPx) {
                if (displayNewPxBand || displayNewPxBandMode) {
                    if (displayNewPxBand && displayNewPxBandMode) {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewPxBand) {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchPX(/*in*/checkCall,/*out*/countryName,pxB4,pxB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchPX(/*in*/ checkCall, /*out*/ countryName, pxB4 ,pxB4BandMode);
                }
            }
            if (displayNewCall) {
                if (displayNewCallBand || displayNewCallBandMode) {
                    if (displayNewCallBand && displayNewCallBandMode) {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/dialFreq,checkMode);
                    } else if (displayNewCallBand) {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/dialFreq);
                    } else {
                        logBook.matchCall(/*in*/checkCall,/*out*/countryName,callB4,callB4BandMode,/*in*/0,checkMode);
                    }
                } else {
                    logBook.matchCall(/*in*/ checkCall, /*out*/ countryName, callB4 ,callB4BandMode);
                }
            }

            if (displayNewCQZ || displayNewITUZ || displayNewDXCC || displayNewCall || displayNewGrid || displayNewPx) {
//Worked
                if ((displayPotential && std_type == 3) || (std_type != 3)) {
                    if (displayWorkedColor) {
                        bgColor = m_config->color_WorkedCall().name();
                    }
                    if (displayWorkedStriked) {
                        strikethrough = true;
                    } else if (displayWorkedUnderlined) {
                        underlined = true;
                    }
                } else if (displayWorkedColor && m_config->otherMessagesMarker()) servis += m_config->color_WorkedCall().name();

                if (displayNewCQZ && !cqzB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewCQZ().name();
                        if (!lotw.isEmpty ()) priority = 31;
                        else priority = 30;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCQZ) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewCQZ().name();
                        if (!lotw.isEmpty ()) priority = 31;
                        else priority = 30;
                        new_marker = true;
                    }
                } else if ((displayNewCQZBand || displayNewCQZBandMode) && !cqzB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewCQZBand().name();
                        if (!lotw.isEmpty ()) priority = 29;
                        else priority = 28;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCQZ) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewCQZBand().name();
                        if (!lotw.isEmpty ()) priority = 29;
                        else priority = 28;
                        new_marker = true;
                    }
                } else if (displayNewITUZ && !ituzB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewITUZ().name();
                        if (!lotw.isEmpty ()) priority = 27;
                        else priority = 26;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewITUZ) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewITUZ().name();
                        if (!lotw.isEmpty ()) priority = 27;
                        else priority = 26;
                        new_marker = true;
                    }
                } else if ((displayNewITUZBand || displayNewITUZBandMode) && !ituzB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewITUZBand().name();
                        if (!lotw.isEmpty ()) priority = 25;
                        else priority = 24;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewITUZ) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewITUZBand().name();
                        if (!lotw.isEmpty ()) priority = 25;
                        else priority = 24;
                        new_marker = true;
                    }
                } else if (displayNewDXCC && !countryB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewDXCC().name();
                        if (!lotw.isEmpty ()) priority = 23;
                        else priority = 22;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewDXCC) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewDXCC().name();
                        if (!lotw.isEmpty ()) priority = 23;
                        else priority = 22;
                        new_marker = true;
                    }
                } else if ((displayNewDXCCBand || displayNewDXCCBandMode) && !countryB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewDXCCBand().name();
                        if (!lotw.isEmpty ()) priority = 21;
                        else priority = 20;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewDXCC) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewDXCCBand().name();
                        if (!lotw.isEmpty ()) priority = 21;
                        else priority = 20;
                        new_marker = true;
                    }
                } else if (displayNewGrid && !gridB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewGrid().name();
                        if (!lotw.isEmpty ()) priority = 16;
                        else priority = 15;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewGrid) {
                            beep = true;
                        }
                    }
                    else  if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewGrid().name();
                        if (!lotw.isEmpty ()) priority = 16;
                        else priority = 15;
                        new_marker = true;
                    }
                } else if ((displayNewGridBand || displayNewGridBandMode) && !gridB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewGridBand().name();
                        if (!lotw.isEmpty ()) priority = 14;
                        else priority = 13;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewGrid) {
                            beep = true;
                        }
                    }
                    else if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewGridBand().name();
                        if (!lotw.isEmpty ()) priority = 14;
                        else priority = 13;
                        new_marker = true;
                    }
                } else  if (displayNewPx && !pxB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewPx().name();
                        if (!lotw.isEmpty ()) priority = 12;
                        else priority = 11;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewPx) {
                            beep = true;
                        }
                    }
                    else  if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewPx().name();
                        if (!lotw.isEmpty ()) priority = 12;
                        else priority = 11;
                        new_marker = true;
                    }
                } else if ((displayNewPxBand || displayNewPxBandMode) && !pxB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewPxBand().name();
                        if (!lotw.isEmpty ()) priority = 10;
                        else priority = 9;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewPx) {
                            beep = true;
                        }
                    }
                    else  if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewPxBand().name();
                        if (!lotw.isEmpty ()) priority = 10;
                        else priority = 9;
                        new_marker = true;
                    }
                } else  if (displayNewCall && !callB4) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewCall().name();
                        if (!lotw.isEmpty ()) priority = 8;
                        else priority = 7;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCall) {
                            beep = true;
                        }
                    }
                    else  if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewCall().name();
                        if (!lotw.isEmpty ()) priority = 8;
                        else priority = 7;
                        new_marker = true;
                    }
                } else if ((displayNewCallBand || displayNewCallBandMode) && !callB4BandMode) {
                    if ((displayPotential && std_type == 3) || std_type != 3) {
                        forceBold = true;
                        bgColor = m_config->color_NewCallBand().name();
                        if (!lotw.isEmpty ()) priority = 6;
                        else priority = 5;
                        strikethrough = false;
                        underlined = false;
                        actwind = true;
                        if (beepOnNewCall) {
                            beep = true;
                        }
                    }
                    else  if (m_config->otherMessagesMarker ()) {
                        servis = servis.left(1) + m_config->color_NewCallBand().name();
                        if (!lotw.isEmpty ()) priority = 6;
                        else priority = 5;
                        new_marker = true;
                    }
                } 
                if (displayWorkedDontShow && std_type != 2 && ((!forceBold && ((displayPotential && std_type == 3) || std_type != 3)) || (!new_marker && m_config->otherMessagesMarker() && std_type == 3))) {
                    show_line = false;
                }
            }
        } else {
            logBook.getDXCC(/*in*/ checkCall, /*out*/ countryName);
        }

        auto items = countryName.split(',');
        mpx = items[1];

        if (!wantedCallList.isEmpty() && (wantedCallList.indexOf(Radio::base_callsign (checkCall)) >= 0 || wantedCallList.indexOf(checkCall) >= 0)) {
            bwantedCall = true;            
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
         
            
        if (displayTxtColor && (displayPotential || std_type != 3)) {
            swpColor = bgColor;
            bgColor = txtColor;
            txtColor = swpColor;
        }
        if (displayCountryName) {
            if (displayCountryPrefix) {
                cntry = items[1];
                
            } else {
                // do some obvious abbreviations, don't care if we using just prefixes here, not big deal to run some replace's
                cntry = items[2];
            }
        }
        if (!bwantedCall && !bwantedPrefix && !bwantedGrid && !bwantedCountry) {
            if (m_config->hideContinents().contains(items[0]) && std_type != 2 && !jt65bc) {
                show_line = false;
            } else if (m_config->enableCountryFilter() && std_type != 2 && !jt65bc) {
                auto countries = m_config->countries ().split(',');
                if (countries.contains(items[1].toUpper()))
                    show_line = false;
            }
            if (show_line && m_config->enableCallsignFilter() && std_type != 2 && !jt65bc) {
                auto callsigns = m_config->callsigns ().split(',');
                if (callsigns.contains(Radio::base_callsign (checkCall)))
                    show_line = false;
            }
        }
    }
    
    if (show_line && decodedText->isNonStd2() && m_config->hidefree() && !decodedText->message().contains(myCall) && std_type != 1 && !jt65bc) {
        show_line = false;
    } else if (show_line && m_config->showcq() && std_type != 1 && std_type != 2 && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (show_line && m_config->showcqrrr73() && std_type != 1 && std_type != 2 && !decodedText->isEnd() && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (show_line && m_config->showcq73() && std_type != 1 && std_type != 2 && !decodedText->isFin() && qAbs(rx_frq-decodedText->frequencyOffset()) >10 && !jt65bc) {
        show_line = false;
    } else if (decodedText->isHint()) {
        if(lotw.isEmpty ())
            servis = "*" + servis.mid(1); // hinted decode
        else
            servis = "°" + servis.mid(1); // lotw hinted decode
    } else if (decodedText->isWrong()) {
        servis = "?" + servis.mid(1); // error decode
    } else if (!lotw.isEmpty ()) {
        servis = "•" + servis.mid(1); // lotw 
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
    if (bypassAllFilters || bypassRxfFilters) {
            show_line = true;
    }
    if (jt65bc) {
        bgColor = "#ffffff";
        txtColor = "#000000";
    }
    if (show_line) {
        if (!checkCall.isEmpty () && (std_type == 1 || std_type == 2 || std_type == 4 || (std_type == 3 && !param.isEmpty()))) {
            qsoHistory.message(checkCall,status,priority,param,tyyp,countryName.left(2),mpx,c_time,decodedText->report(),decodedText->frequencyOffset(),checkMode);
        } 
        if (std_type == 2) {
            if(!m_config->redMarker()) std_type = 0;
            else if(m_config->blueMarker() && !hisCall.isEmpty () && checkCall.contains(hisCall)) std_type = 5;
        }
        appendText(messageText, bgColor, txtColor, std_type, servis, cntry, forceBold, strikethrough, underlined, decodedText->isDXped());
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

        auto const& parts = txs.at(i).split (' ', QString::SkipEmptyParts);
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
        if (wastx_ && ttime - last_tx < 2 && m_config->hide_TX_messages())
            appendText(t,bg,"#000000",0," "," ",false,false,false,false,true);
        else
            appendText(t,bg);
    }
    wastx_ = true;
    last_tx = ttime;
}

void DisplayText::displayQSY(QString text)
{
  QString t = QDateTime::currentDateTimeUtc().toString("hhmmss") + "            " + text;
  QString bg="#ff69b4";
  appendText(t,bg);
}

