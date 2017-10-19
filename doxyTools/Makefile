#- Output directories --------------------------------------------------

DERIVED  := ../../derived/warp
DEPDIR   := $(DERIVED)/dep
OBJDIR   := $(DERIVED)/obj
EXEDIR   := $(DERIVED)/exe
DOCDIR   := $(DERIVED)/doc

$(shell rm -fr $(DERIVED)/* >/dev/null)
$(shell mkdir -p $(DEPDIR) $(OBJDIR) $(EXEDIR) $(DOCDIR) )
$(shell rm -f $(DEPDIR)/$*.Td $(EXEDIR)/* >/dev/null)


#- Target images -------------------------------------------------------

DEFAULT  := $(EXEDIR)/warp
UNITTEST := $(EXEDIR)/gt


#- Source areas and search paths ---------------------------------------

SCRIPTS  := ../scripts

GTEST    := ../gtest
VPATH    := $(GTEST)

INCL = \
-I ../cg_ir/export/include \
-I ../stubit \
-I $(GTEST)/gtest-1.7.0/include/gtest \
-I $(GTEST)/gtest-1.7.0/src \


#- Compilation recipes -------------------------------------------------

DEPFLAGS.cpp    = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.cpp.Td
COMPILE.cpp     = g++ -x c++ -std=c++11 -o $(OBJDIR)/$*.cpp.o $(INCL) $(DEPFLAGS.cpp) -c
POSTCOMPILE.cpp = mv -f $(DEPDIR)/$*.cpp.Td $(DEPDIR)/$*.cpp.d

DEPFLAGS.gt     = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.gt.Td
COMPILE.gt      = g++ -x c++ -std=c++11 -o $(OBJDIR)/$*.gt.o $(INCL) $(DEPFLAGS.gt) -c
POSTCOMPILE.gt  = mv -f $(DEPDIR)/$*.gt.Td $(DEPDIR)/$*.gt.d


#- Main library image --------------------------------------------------

SRCS := $(wildcard *.cpp)
OBJS := $(patsubst %.cpp,$(OBJDIR)/%.cpp.o,$(SRCS))

$(DEFAULT) : $(OBJS)
	g++ -o $(DEFAULT) $(OBJS)
	$(DEFAULT)

$(OBJDIR)/%.cpp.o : %.cpp
$(OBJDIR)/%.cpp.o : %.cpp $(DEPDIR)/%.cpp.d
	$(COMPILE.cpp) $<
	$(POSTCOMPILE.cpp)


#- Package tests -------------------------------------------------------

GTSRCS := $(wildcard gt/*.gt)
GTOBJS := $(patsubst gt/%.gt,$(OBJDIR)/%.gt.o,$(GTSRCS))

gt : $(UNITTEST)

$(UNITTEST) : $(OBJDIR)/gtmain.cpp.o $(GTOBJS)
	g++ -o $(UNITTEST) $(OBJDIR)/gtmain.cpp.o $(GTOBJS) -lpthread
	$(UNITTEST)

$(OBJDIR)/%.gt.o : gt/%.gt
$(OBJDIR)/%.gt.o : gt/%.gt $(DEPDIR)/%.gt.d
	$(COMPILE.gt) $<
	$(POSTCOMPILE.gt)


#- Automatic depenedency tracking --------------------------------------

$(DEPDIR)/%.d: ;
.PRECIOUS: $(DEPDIR)/%.d

include $(wildcard $(patsubst %,$(DEPDIR)/%.d,$(basename $(SRCS) $(GTSRCS))))


#- Doxygen-based documentation -----------------------------------------

doc :
	$(SCRIPTS)/doxygen.sh $(DOCDIR) $(wildcard *.doxy)

