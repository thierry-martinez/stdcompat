properties([
  [
    $class: 'ThrottleJobProperty',
    categories: ['category'],
    limitOneJobWithMatchingParams: false,
    maxConcurrentPerNode: 4,
    maxConcurrentTotal: 0,
    paramsToUseForLimit: '',
    throttleEnabled: true,
    throttleOption: 'category'
  ],
])

pipeline {
    agent none

    stages {
        stage('Prepare') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker build -t stdcompat .'
            }
        }
        stage('Bootstrap') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker run --rm --volume $PWD:/workspace stdcompat sh -c \'cd /workspace && eval `opam config env` && make -f Makefile.bootstrap\''
                stash name: 'bootstrap'
                sh 'ls -R'
            }
        }
        stage('Build') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'bootstrap'
                sh 'docker run --rm --volume $PWD:/workspace stdcompat sh -c \'cd /workspace && mkdir build && cd build && ../configure && make\''
                sh 'ls -R'
            }
        }
        stage('Test with magic') {
            agent {
                label 'linux'
            }
            steps {
                script {
                    def switches = sh (
                        script: 'docker run --rm stdcompat opam switch -s',
                        returnStdout: true
                    ).split('\n')
                    def branches = [:]
                    for (i in switches) {
                        def switch_name = i
                        branches[switch_name] = {
                            node('linux') {
                                sh "rm -rf build"
                                unstash 'bootstrap'
                                sh "docker run --rm --volume \$PWD:/workspace stdcompat sh -c 'cd /workspace && opam config exec --switch $switch_name -- sh -c '\\''mkdir build && cd build && ../../configure && make && make tests && ../configure && make && make tests'\\'"
                            }
                        }
                    }
                    throttle(['category']) {
                        parallel branches
                    }
                }
            }
        }
        stage('Test without magic') {
            agent {
                label 'linux'
            }
            steps {
                script {
                    def switches = sh (
                        script: 'docker run --rm stdcompat opam switch -s',
                        returnStdout: true
                    ).split('\n')
                    def branches = [:]
                    for (i in switches) {
                        def switch_name = i
                        branches[switch_name] = {
                            node('linux') {
                                sh "rm -rf build"
                                unstash 'bootstrap'
                                sh "docker run --rm --volume \$PWD:/workspace stdcompat sh -c 'cd /workspace && opam config exec --switch $switch_name -- sh -c '\\''mkdir build && cd build && ../../configure && make && make tests && ../configure --disable-magic && make && make tests'\\'"
                            }
                        }
                    }
                    throttle(['category']) {
                        parallel branches
                    }
                }
            }
        }
        stage('Test windows') {
            agent {
                label 'windows'
            }
            steps {
                bat 'echo foo'
            }
        }
        stage('Deploy') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'build'
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
