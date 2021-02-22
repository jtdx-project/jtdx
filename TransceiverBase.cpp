#include "TransceiverBase.hpp"

#include <exception>

#include <QString>
#include <QTimer>
#include <QThread>
#include <QDebug>

#include "moc_TransceiverBase.cpp"

namespace
{
  auto const unexpected = TransceiverBase::tr ("Unexpected rig error");
}

void TransceiverBase::start (unsigned sequence_number,JTDXDateTime * jtdxdatetime) noexcept
{
  QString message;
  try
    {
      last_sequence_number_ = sequence_number;
      jtdxtime_ = jtdxdatetime;
#if JTDX_DEBUG_TO_FILE
      FILE * pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever start\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
      fclose (pFile);
#endif
      may_update u {this, true};
      shutdown ();
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever start shutdown done\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
      fclose (pFile);
#endif
      startup ();
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever start done\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
      fclose (pFile);
#endif
    }
  catch (std::exception const& e)
    {
      message = e.what ();
    }
  catch (...)
    {
      message = unexpected;
    }
  if (!message.isEmpty ())
    {
      offline (message);
    }
}

void TransceiverBase::set (TransceiverState const& s,
                           unsigned sequence_number) noexcept
{
  TRACE_CAT ("TransceiverBase", "#:" << sequence_number << s);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Transiever set state %d #:%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.online(),sequence_number);
  fclose (pFile);
  auto ms = jtdxtime_->currentMSecsSinceEpoch2();
#endif
  QString message;
  try
    {
      last_sequence_number_ = sequence_number;
      may_update u {this, true};
      bool was_online {requested_.online ()};
      if (!s.online () && was_online)
        {
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf(pFile,"%s Transceiver shutdown",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
          fclose (pFile);
#endif
          shutdown ();
        }
      else if (s.online () && !was_online)
        {
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf(pFile,"%s restart shutdown ",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
          fclose (pFile);
#endif
          shutdown ();
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf(pFile,"%s start",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
          fclose (pFile);
#endif
          startup ();
        }
      if (requested_.online ())
        {
          bool ptt_on {false};
          bool ptt_off {false};
          if (requested_.blocksize() != s.blocksize()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing blocksize=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.blocksize());
            fclose (pFile);
#endif
            do_blocksize (s.blocksize());
            requested_.blocksize (s.blocksize ());
          }
          if (requested_.period() != s.period()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing period=%0.1f\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.period());
            fclose (pFile);
#endif
            do_period (s.period());
            requested_.period (s.period ());
          }
          if (requested_.spread() != s.spread()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing spread=%0.1f\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.spread());
            fclose (pFile);
#endif
            do_spread (s.spread());
            requested_.spread (s.spread ());
          }
          if (requested_.nsym() != s.nsym()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing nsym=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.nsym());
            fclose (pFile);
#endif
            do_nsym (s.nsym());
            requested_.nsym (s.nsym ());
          }
          if (requested_.trfrequency() != s.trfrequency()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing trfrequency=%0.1f\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.trfrequency());
            fclose (pFile);
#endif
            do_trfrequency (s.trfrequency());
            requested_.trfrequency (s.trfrequency ());
          }
          if (requested_.volume() != s.volume()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing volume=%0.1f\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.volume());
            fclose (pFile);
#endif
            do_txvolume (s.volume());
            requested_.volume (s.volume ());
          }
          if (requested_.audio() != s.audio()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing audio=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.audio());
            fclose (pFile);
#endif
            do_audio (s.audio());
            requested_.audio (s.audio ());
          }
          if (requested_.tx_audio() != s.tx_audio()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing tx_audio=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.audio());
#endif
            if (s.tx_audio()) {
              do_modulator_start(s.symbolslength (), s.framespersymbol (), s.trfrequency (), s.tonespacing (), s.synchronize (),s.dbsnr (), s.trperiod ());
              requested_.symbolslength(s.symbolslength ());
              requested_.framespersymbol(s.framespersymbol ());
              requested_.trfrequency(s.trfrequency ());
              requested_.tonespacing(s.tonespacing ());
              requested_.synchronize(s.synchronize ());
              requested_.dbsnr(s.dbsnr ());
              requested_.trperiod(s.trperiod ());
            } else {
              do_modulator_stop(s.quick ());
              requested_.quick (s.quick ());
            }

#if JTDX_DEBUG_TO_FILE
            fclose (pFile);
#endif
            requested_.tx_audio (s.tx_audio ());
          }


          if (requested_.tune() != s.tune()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing tune=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.tune());
            fclose (pFile);
#endif
            do_tune (s.tune());
            requested_.tune (s.tune ());
          }
          if (requested_.ft4_mode() != s.ft4_mode()) {
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing ft4_mode=%d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.ft4_mode());
            fclose (pFile);
#endif
            do_post_ft4_mode (s.ft4_mode());
            requested_.ft4_mode (s.ft4_mode ());
          }
          if (s.ptt () != requested_.ptt ())
            {
              ptt_on = s.ptt ();
              ptt_off = !s.ptt ();
            }
          if (ptt_off)
            {
#if JTDX_DEBUG_TO_FILE
              pFile = fopen (debug_file_.c_str(),"a");
              fprintf(pFile,"%s Timing ptt_off\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
              fclose (pFile);
#endif
              do_ptt (false);
              do_post_ptt (false);
              QThread::msleep (100); // some rigs cannot process CAT
                                     // commands while switching from
                                     // Tx to Rx
            }
          if (s.frequency ()    // ignore bogus zero frequencies
              && ((s.frequency () != requested_.frequency () // and QSY
                   || (s.mode () != UNK && s.mode () != requested_.mode ())) // or mode change
                  || ptt_off))       // or just returned to rx
            {
              auto ms2 = jtdxtime_->currentMSecsSinceEpoch2();
#if JTDX_DEBUG_TO_FILE
              pFile = fopen (debug_file_.c_str(),"a");
              fprintf(pFile,"%s Timing do_frequency %lld\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.frequency ());
              fclose (pFile);
#endif
              do_frequency (s.frequency (), s.mode (), ptt_off);
              do_post_frequency (s.frequency (), s.mode ());

              // record what actually changed
              requested_.frequency (actual_.frequency ());
              requested_.mode (actual_.mode ());
              set_freq_time = jtdxtime_->currentMSecsSinceEpoch2() - ms2;
            }
          if (!s.tx_frequency ()
              || (s.tx_frequency () > 10000 // ignore bogus startup values
                  && s.tx_frequency () < std::numeric_limits<Frequency>::max () - 10000))
            {
              if ((s.tx_frequency () != requested_.tx_frequency () // and QSY
                   || (s.mode () != UNK && s.mode () != requested_.mode ())) // or mode change
                  // || s.split () != requested_.split ())) // or split change
                  || (s.tx_frequency () && ptt_on)) // or about to tx split
                {
#if JTDX_DEBUG_TO_FILE
                  pFile = fopen (debug_file_.c_str(),"a");
                  fprintf(pFile,"%s Timing do_tx_frequency %lld\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),s.frequency ());
                  fclose (pFile);
#endif
                  do_tx_frequency (s.tx_frequency (), s.mode (), ptt_on);
                  do_post_tx_frequency (s.tx_frequency (), s.mode ());

                  // record what actually changed
                  requested_.tx_frequency (actual_.tx_frequency ());
                  requested_.split (actual_.split ());
                }
            }
          if (ptt_on)
            {
#if JTDX_DEBUG_TO_FILE
              pFile = fopen (debug_file_.c_str(),"a");
              fprintf(pFile,"%s Timing ptt_on\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
              fclose (pFile);
#endif
              do_ptt (true);
              do_post_ptt (true);
//              QThread::msleep (100 + ms); // some rigs cannot process CAT
                                     // commands while switching from
                                     // Rx to Tx
            }

          // record what actually changed
          requested_.ptt (actual_.ptt ());
        } else {
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s NOT ONLINE\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
          fclose (pFile);
#endif
        }
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever set end %lld ms.\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->currentMSecsSinceEpoch2() - ms);
      fclose (pFile);
#endif
    }
  catch (std::exception const& e)
    {
      message = e.what ();
    }
  catch (...)
    {
      message = unexpected;
    }
  
  if (!message.isEmpty ())
    {
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever set message %s\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),message.toStdString().c_str());
      fclose (pFile);
#endif
      offline (message);
    }
}

void TransceiverBase::startup ()
{
  QString message;
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Transiever startup\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
  fclose (pFile);
  auto ms = jtdxtime_->currentMSecsSinceEpoch2();
#endif
  try
    {
      actual_.online (true);
      requested_.online (true);
      auto res = do_start (jtdxtime_);
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Timing do_start\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
      fclose (pFile);
#endif
      do_post_start (jtdxtime_);
      Q_EMIT resolution (res);
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever startup end %lld ms.\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->currentMSecsSinceEpoch2() - ms);
      fclose (pFile);
#endif
    }
  catch (std::exception const& e)
    {
      message = e.what ();
    }
  catch (...)
    {
      message = unexpected;
    }
  if (!message.isEmpty ())
    {
#if JTDX_DEBUG_TO_FILE
      pFile = fopen (debug_file_.c_str(),"a");
      fprintf(pFile,"%s Transiever startup message %s\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),message.toStdString().c_str());
      fclose (pFile);
#endif
      offline (message);
    }
}

void TransceiverBase::shutdown ()
{
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Transiever shutdown\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
  fclose (pFile);
  auto ms = jtdxtime_->currentMSecsSinceEpoch2();
#endif
  if (requested_.online ())
    {
      may_update u {this};
      try
        {
          // try and ensure PTT isn't left set
          do_ptt (false);
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf(pFile,"%s Timing do_ptt:0\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
          fclose (pFile);
#endif
          do_post_ptt (false);
          if (requested_.split ())
            {
              // try and reset split mode
              do_tx_frequency (0, UNK, true);
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf(pFile,"%s Timing do_tx_freq:0\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
            fclose (pFile);
#endif
              do_post_tx_frequency (0, UNK);
            }
        }
      catch (...)
        {
          // don't care about exceptions
        }
    }
  do_stop ();
#if JTDX_DEBUG_TO_FILE
  pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Timing do_stop\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
  fclose (pFile);
#endif
  do_post_stop ();
  actual_ = TransceiverState {};
  requested_ = TransceiverState {};
#if JTDX_DEBUG_TO_FILE
  pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Transiever shutdown end %lld ms.\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->currentMSecsSinceEpoch2() - ms);
  fclose (pFile);
#endif
}

void TransceiverBase::stop () noexcept
{
  QString message;
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf(pFile,"%s Transiever stop\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str());
  fclose (pFile);
#endif
  try
    {
      shutdown ();
    }
  catch (std::exception const& e)
    {
      message = e.what ();
    }
  catch (...)
    {
      message = unexpected;
    }
  if (!message.isEmpty ())
    {
      offline (message);
    }
  else
    {
      Q_EMIT finished ();
    }
}

void TransceiverBase::update_rx_frequency (Frequency rx)
{
  if (rx)
    {
      actual_.frequency (rx);
      requested_.frequency (rx);    // track rig changes
    }
}

void TransceiverBase::update_other_frequency (Frequency tx)
{
  actual_.tx_frequency (tx);
}

void TransceiverBase::update_split (bool state)
{
  actual_.split (state);
}

void TransceiverBase::update_mode (MODE m)
{
  actual_.mode (m);
  requested_.mode (m);    // track rig changes
}

void TransceiverBase::update_PTT (bool state)
{
  actual_.ptt (state);
}

void TransceiverBase::update_level (int l)
{
  actual_.level (l);
//  requested_.level (l);    // track rig changes
}

void TransceiverBase::update_power (unsigned int p)
{
  actual_.power (p);
//  requested_.power (p);    // track rig changes
}

void TransceiverBase::update_complete (bool force_signal)
{
  if ((do_pre_update () && actual_ != last_) || force_signal)
    {
      Q_EMIT update (actual_, last_sequence_number_);
      last_ = actual_;
    }
}

void TransceiverBase::offline (QString const& reason)
{
  Q_EMIT failure (reason);
  try
    {
      shutdown ();
    }
  catch (...)
    {
      // don't care
    }
}
