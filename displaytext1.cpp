// This source code file was last time modified by Igor Chernikov UA3DJY on May 25th, 2016.
// All changes are shown in the patch file coming together with the full JTDX source code.

#include "displaytext.h"

#include <QMouseEvent>
#include <QDateTime>
#include <QTextCharFormat>
#include <QFont>
#include <QTextCursor>

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

void DisplayText::setContentFont(QFont const& font)
{
  setFont (font);
  m_charFormat.setFont (font);
  selectAll ();
  auto cursor = textCursor ();
  cursor.mergeCharFormat (m_charFormat);
  cursor.clearSelection ();
  cursor.movePosition (QTextCursor::End);

  // position so viewport scrolled to left
  cursor.movePosition (QTextCursor::Up);
  cursor.movePosition (QTextCursor::StartOfLine);

  setTextCursor (cursor);
  ensureCursorVisible ();
}

void DisplayText::mouseDoubleClickEvent(QMouseEvent *e)
{
  bool ctrl = (e->modifiers() & Qt::ControlModifier);
  bool shift = (e->modifiers() & Qt::ShiftModifier);
  emit(selectCallsign(shift,ctrl));
  QTextEdit::mouseDoubleClickEvent(e);
}

void DisplayText::insertLineSpacer(QString const& line)
{
  appendText (line, "#d3d3d3");
}

void DisplayText::appendText(QString const& text, QString const& bg)
{
    QString escaped {text.trimmed().replace('<',"&lt;").replace('>',"&gt;").replace(' ', "&nbsp;")};
    QString s = "<table border=0 cellspacing=0 width=100%><tr><td bgcolor=\"" +
    bg + "\">" + escaped + "</td></tr></table>";
    auto cursor = textCursor ();
    cursor.movePosition (QTextCursor::End);
    auto pos = cursor.position ();
    cursor.insertHtml (s);
    cursor.setPosition (pos, QTextCursor::MoveAnchor);
    cursor.movePosition (QTextCursor::End, QTextCursor::KeepAnchor);
    cursor.mergeCharFormat (m_charFormat);
    cursor.clearSelection ();

    // position so viewport scrolled to left
    cursor.movePosition (QTextCursor::Up);
    cursor.movePosition (QTextCursor::StartOfLine);
    setTextCursor (cursor);
    ensureCursorVisible ();
}


void DisplayText::_appendDXCCWorkedB4(DecodedText& t1, QString& bg,
                                      LogBook logBook, QColor color_CQ,
                                      QColor color_DXCC,
                                      QColor color_NewCall)
{
    QString call = t1.CQersCall ();

    QString countryName;
    bool callWorkedBefore;
    bool countryWorkedBefore;
    logBook.match(/*in*/call,/*out*/countryName,callWorkedBefore,countryWorkedBefore);

    int charsAvail = 48;

    bool hint = t1.isHint();

    // the decoder (seems) to always generate 40 chars. For a normal CQ call, the last five are spaces
    // TODO this magic 36 characters is also referenced in MainWindow::doubleClickOnCall()
    int nmin=35;
    int i=t1.indexOf(" CQ ");
    int k=t1.string().mid(i+4,3).toInt();
    if(k>0 and k<999) nmin += 4;
    int s3 = t1.indexOf(" ",nmin);
    if (s3 < nmin) s3 = nmin; // always want at least the characters to position 35
    s3 += 1; // convert the index into a character count
    t1 = t1.left(s3);  // reduce trailing white space
    charsAvail -= s3;

    if (charsAvail > 4)
    {
        if (!countryWorkedBefore) // therefore not worked call either
        {
            if(hint)  {
                  t1 += "* !";
                  bg=color_DXCC.name();
                }
				  else {
                  t1 += "  !";
                  bg=color_DXCC.name();
				  }
            }
        else
            if (!callWorkedBefore) // but have worked the country
            {
                if(hint)  {
                     t1 += "* ~";
                     bg=color_NewCall.name();
                }
                else {
                     t1 += "  ~";
                     bg=color_NewCall.name();
                } 
            }
            else
            {
                if(hint)  {
                     t1 += "*  ";
                     bg=color_CQ.name();
				}
                else {
                     t1 += "   ";  // have worked this call before
                     bg=color_CQ.name();
                } 
            }
        charsAvail -= 3;

        // do some obvious abbreviations
        countryName.replace ("Islands", "Is.");
        countryName.replace ("Island", "Is.");
        countryName.replace ("North ", "N. ");
        countryName.replace ("Northern ", "N. ");
        countryName.replace ("South ", "S. ");
        countryName.replace ("East ", "E. ");
        countryName.replace ("Eastern ", "E. ");
        countryName.replace ("West ", "W. ");
        countryName.replace ("Western ", "W. ");
        countryName.replace ("Central ", "C. ");
        countryName.replace (" and ", " & ");
        countryName.replace ("Republic", "Rep.");
        countryName.replace ("United States", "U.S.A.");
        countryName.replace ("Fed. Rep. of ", "");
        countryName.replace ("French ", "Fr.");
        countryName.replace ("Asiatic", "AS");
        countryName.replace ("European", "EU");
        countryName.replace ("African", "AF");
        t1 += countryName;
    }
}

void DisplayText::displayDecodedText(DecodedText decodedText, QString myCall,
                                     bool displayCountryName, LogBook logBook,
                                     QColor color_CQ, QColor color_MyCall,
                                     QColor color_DXCC, QColor color_NewCall)
{
    QString bg="white";
    bool CQcall = false;
    if (decodedText.string ().contains (" CQ ") > 0 || decodedText.string ().contains (" CQ DX "))
    {
        CQcall = true;
        bg=color_CQ.name();
    }
    if (myCall != "" and (
                          decodedText.indexOf (" " + myCall + " ") >= 0
                          or decodedText.indexOf (" " + myCall + "/") >= 0
                          or decodedText.indexOf ("/" + myCall + " ") >= 0
                          ))
      bg=color_MyCall.name();

    // if enabled add the DXCC entity and B4 status to the end of the preformated text line t1
    if (displayCountryName && CQcall)
        _appendDXCCWorkedB4(/*mod*/decodedText,bg,logBook,color_CQ,
                            color_DXCC,color_NewCall);
    appendText(decodedText.string(),bg);
}


void DisplayText::displayTransmittedText(QString text, QString modeTx, qint32 txFreq,
                                         QColor color_TxMsg, bool bFastMode)
{
    QString bg=color_TxMsg.name();
    QString t1=" @ ";
    if(modeTx=="JT65") t1=" # ";
    if(modeTx=="JTMSK") t1=" & ";
    QString t2;
    t2.sprintf("%4d",txFreq);
    QString t;
    if(bFastMode) {
      t = QDateTime::currentDateTimeUtc().toString("hhmmss") + \
        "  Tx      " + t2 + t1 + text;
    } else {
      t = QDateTime::currentDateTimeUtc().toString("hhmm") + \
        "  Tx      " + t2 + t1 + text;
    }
    appendText(t,bg);
}

void DisplayText::displayQSY(QString text)
{
  QString t = QDateTime::currentDateTimeUtc().toString("hhmmss") + "            " + text;
  QString bg="hot pink";
  appendText(t,bg);
}
