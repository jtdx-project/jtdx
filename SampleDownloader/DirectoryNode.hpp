#ifndef DIRECTORY_NODE_HPP__
#define DIRECTORY_NODE_HPP__

#include <QTreeWidgetItem>
#include <QString>

//
// Tree widget item representing a file system directory.
//
// It  renders the  directory name  in the  first column  and progress
// information in the 2nd column. The progress information consists of
// two 64 bit integer values, the 1st in the DisplayRole is the number
// of  bytes received  and the  2nd in  the UserRole  the total  bytes
// expected.    The   progress   information  is   not   automatically
// maintained,  see the  Directory  class  for an  example  of how  to
// dynamically maintain  the DirectoryNode  progress values.   The 1st
// column also  renders a tristate  check box that controls  the first
// column check boxes of child items.
//
class DirectoryNode final
  : public QTreeWidgetItem
{
public:
  explicit DirectoryNode (QTreeWidgetItem * parent, QString const& name)
    : QTreeWidgetItem {parent, Type}
  {
    setFlags (flags () | Qt::ItemIsUserCheckable | Qt::ItemIsTristate);
    setText (0, name);
    setCheckState (0, Qt::Unchecked);

    // initialize as empty, the owning QTreeWidget must maintain these
    // progress values
    setData (1, Qt::DisplayRole, 0ll); // progress in bytes
    setData (1, Qt::UserRole, 0ll);    // expected bytes
  }

  bool operator == (QString const& name) const
  {
    return name == text (0);
  }

  static int const Type {UserType};
};

inline
bool operator == (QString const& lhs, DirectoryNode const& rhs)
{
  return rhs == lhs;
}

#endif
