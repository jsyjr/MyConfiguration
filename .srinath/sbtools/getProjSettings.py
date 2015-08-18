#!/usr/bin/env python

import xml.dom.minidom
import os
from os import path
from pathUtils import searchUpFor

class Solution:
    def __init__(self):
        self.projects = []
        self.rootDir = ''

    def __str__(self):
        return '\n'.join(['%s' % p for p in self.projects])

    def getProjByName(self, name):
        for proj in self.projects:
            if name == proj.name:
                return proj

        return None

    def setRootDir(self, rootDir):
        self.rootDir = rootDir
        for p in self.projects:
            p.rootDir = rootDir

class Project:
    def __init__(self, name, depends):
        self.name = name
        self.includes = []
        self.exports = []
        self.depends = depends
        self.path = ''
        self.rootDir = ''

    def includesFile(self, fname):
        for inc in self.includes:
            incPath = path.abspath(path.join(self.rootDir, inc['path']))
            if incPath.lower() in path.abspath(fname).lower():
                return True

        return False

    def addInclude(self, path, pattern):
        self.includes.append({'path': path, 'pattern': pattern, 'tagsFile': '%s.inc.tags' % self.name})

    def addExport(self, path, pattern):
        self.exports.append({'path': path, 'pattern': pattern, 'tagsFile': '%s.exp.tags' % self.name})

    def __str__(self):
        return ("%s (%s):"
                "\n   includes: %s"
                "\n   exports: %s"
                "\n   depends: %s") % (self.name, self.path, self.includes, self.exports, ' '.join(self.depends))

def getText(doms):
    txt = ''
    for d in doms:
        for c in d.childNodes:
            if c.nodeType == c.TEXT_NODE:
                txt += c.data
    return txt

def handleProj(dom):
    name = dom.getAttribute('name')
    depends = getText(dom.getElementsByTagName('depends')).strip().split()
    proj = Project(name, depends)

    for inc_dom in dom.getElementsByTagName('include'):
        proj.addInclude(inc_dom.getAttribute('path'), inc_dom.getAttribute('pattern'))

    for exp_dom in dom.getElementsByTagName('export'):
        proj.addExport(exp_dom.getAttribute('path'), exp_dom.getAttribute('pattern'))

    return proj

def handleModule(dom):
    modPath = dom.getAttribute('path')
    depends = []

    name = path.basename(modPath)
    proj = Project(name, depends)
    proj.path = modPath

    incPattern = '*.[ch]pp *.c *.h'

    extraIncludes = dom.getAttribute('extraIncludes')
    if extraIncludes:
        incPattern += (' %s' % extraIncludes)

    proj.addInclude(modPath, '*.[ch]pp *.h')
    proj.addExport(path.join(modPath, 'export'), '*.hpp *.h')

    return proj

def addModuleDependencies(modules, rootDir):
    moduleNames = set()
    [moduleNames.add(mod.name) for mod in modules]

    for mod in modules:
        if not mod.path:
            continue

        dependencyFile = path.join(rootDir, mod.path, 'MODULE_DEPENDENCIES')
        if not path.isfile(dependencyFile):
            continue

        for depName in open(dependencyFile).read().splitlines():
            if depName.startswith('#'):
                continue

            if depName.startswith('='):
                depName = depName[1:]

            if depName in moduleNames:
                mod.depends.append(depName)
            elif depName.startswith('libmw'):
                depName = depName.replace('libmw', '', 1)
                if depName in moduleNames:
                    mod.depends.append(depName)

def handleSolution(dom):
    soln = Solution()

    project_doms = dom.getElementsByTagName('project')
    for proj_dom in project_doms:
        soln.projects.append(handleProj(proj_dom))

    modules = dom.getElementsByTagName('module')
    for module in modules:
        soln.projects.append(handleModule(module))

    return soln

def getProjSettings():
    projSpecFile = searchUpFor('.vimproj.xml')
    if not projSpecFile:
        homePath = path.join(os.environ['HOME'], '.vimproj.xml')
        if path.exists(homePath):
            projSpecFile = homePath

    if projSpecFile:
        dom = xml.dom.minidom.parseString(open(projSpecFile).read())
        spec = handleSolution(dom)
        battree = searchUpFor('battree')
        if battree:
            addModuleDependencies(spec.projects, path.dirname(battree))

        return spec
    else:
        return None

if __name__ == "__main__":
    print getProjSettings()
